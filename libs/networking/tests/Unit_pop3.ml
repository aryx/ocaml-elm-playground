(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_pop3.mli *)

let lines = Alcotest.(check (list string))

let replay (c : Pop3.client) (server : string list) : Pop3.client * string list =
  List.fold_left (fun (c, sent) l -> let c, out = Pop3.step c l in (c, sent @ out)) (c, []) server

let fetched = Alcotest.(option (result (list (pair string string)) string))
let one = "Subject: one\n\n.a line with a dot\n"
let two = "Subject: two\n\nhello\n"

let tests =
  Testo.categorize "Pop3"
    [
      Testo.create "RFC 1939's session, section 10: fetched, deleted, QUIT" (fun () ->
          let server =
            [ "+OK POP3 server ready <1896.697170952@dbc.mtview.ca.us>"; "+OK"; "+OK mrose's maildrop has 2 messages (320 octets)"; "+OK 2 320";
              "+OK 2 messages (320 octets)"; "1 120"; "2 200"; "." ]
            @ ("+OK 120 octets" :: Pop3.stuff one) @ [ "+OK message 1 deleted" ]
            @ ("+OK 200 octets" :: Pop3.stuff two) @ [ "+OK message 2 deleted"; "+OK dewey POP3 server signing off (maildrop empty)" ]
          in
          let c, sent = replay (Pop3.client ~user:"mrose" ~pass:"tanstaaf" ~leave:false ~known:[] ()) server in
          lines "what the client said" [ "USER mrose"; "PASS tanstaaf"; "STAT"; "LIST"; "RETR 1"; "DELE 1"; "RETR 2"; "DELE 2"; "QUIT" ] sent;
          Alcotest.check fetched "both, the dot taken off" (Some (Ok [ ("", one); ("", two) ])) (Pop3.finished c));
      Testo.create "leave mail on server: UIDL, only the new, no DELE" (fun () ->
          let server = [ "+OK ready"; "+OK"; "+OK"; "+OK 2 320"; "+OK"; "1 whqtswO00WBw418f9t5JxYwZ"; "2 QhdPYR:00WBw1Ph7x7"; "." ] @ ("+OK" :: Pop3.stuff two) @ [ "+OK bye" ] in
          let c, sent = replay (Pop3.client ~user:"bob" ~pass:"x" ~leave:true ~known:[ "whqtswO00WBw418f9t5JxYwZ" ] ()) server in
          lines "no DELE" [ "USER bob"; "PASS x"; "STAT"; "UIDL"; "RETR 2"; "QUIT" ] sent;
          Alcotest.check fetched "the new one, with its id" (Some (Ok [ ("QhdPYR:00WBw1Ph7x7", two) ])) (Pop3.finished c));
      Testo.create "a limit: only the newest" (fun () ->
          let server = [ "+OK"; "+OK"; "+OK"; "+OK 3 3"; "+OK"; "1 a"; "2 b"; "3 c"; "." ] @ ("+OK" :: Pop3.stuff two) @ [ "+OK bye" ] in
          let _, sent = replay (Pop3.client ~user:"bob" ~pass:"x" ~leave:true ~known:[] ~limit:1 ()) server in
          lines "the last one" [ "USER bob"; "PASS x"; "STAT"; "UIDL"; "RETR 3"; "QUIT" ] sent);
      Testo.create "a wrong password: -ERR, and QUIT" (fun () ->
          let c, sent = replay (Pop3.client ~user:"bob" ~pass:"no" ~leave:false ~known:[] ()) [ "+OK ready"; "+OK"; "-ERR invalid password"; "+OK bye" ] in
          lines "QUIT after" [ "USER bob"; "PASS no"; "QUIT" ] sent;
          Alcotest.check fetched "refused" (Some (Error "invalid password")) (Pop3.finished c));
      Testo.create "an empty maildrop: STAT, QUIT" (fun () ->
          let c, sent = replay (Pop3.client ~user:"bob" ~pass:"x" ~leave:false ~known:[] ()) [ "+OK"; "+OK"; "+OK"; "+OK 0 0"; "+OK bye" ] in
          lines "nothing asked" [ "USER bob"; "PASS x"; "STAT"; "QUIT" ] sent;
          Alcotest.check fetched "nothing" (Some (Ok [])) (Pop3.finished c));
    ]
