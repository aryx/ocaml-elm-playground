(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_irc_server.mli *)

(* the server's event loop and the clients' frames, a few milliseconds:
 * the IRC lines each client received, parsed *)
let pump (irc : Irc_server.t) (clients : Transport.t list) (n : int) : Irc.message list list =
  let got = Array.make (List.length clients) [] in
  for _ = 1 to n do
    Irc_server.step irc;
    List.iteri
      (fun i (c : Transport.t) ->
        got.(i) <- got.(i) @ List.filter_map (fun l -> Result.to_option (Irc.parse l)) (c.receive ()))
      clients;
    Unix.sleepf 0.001
  done;
  Array.to_list got

let say (c : Transport.t) (line : string) : unit = c.send line
let commands (ms : Irc.message list) : string list = List.map (fun (m : Irc.message) -> m.command) ms

let tests (caps : < Cap.network ; .. >) =
  Testo.categorize "Irc_server"
    [
      Testo.create "two users in #ocaml" (fun () ->
          let irc, port = Irc_server.create caps ~port:0 () in
          let a = Relay_client.connect caps ~host:"127.0.0.1" ~port and b = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          say a "NICK alice";
          say a "USER alice 0 * :Alice";
          say b "NICK alice";
          let got = pump irc [ a; b ] 50 in
          Alcotest.(check (list string)) "alice welcomed" [ "001" ] (commands (List.nth got 0));
          Alcotest.(check (list string)) "bob told the nick is taken" [ "433" ] (commands (List.nth got 1));
          say b "NICK bob";
          say b "USER bob 0 * :Bob";
          ignore (pump irc [ a; b ] 50);
          Alcotest.(check (list string)) "the nicks" [ "alice"; "bob" ] (Irc_server.nicks irc);
          say a "JOIN #ocaml";
          ignore (pump irc [ a; b ] 50);
          say b "JOIN #ocaml";
          let got = pump irc [ a; b ] 50 in
          Alcotest.(check (list string)) "alice sees bob join" [ "JOIN" ] (commands (List.nth got 0));
          Alcotest.(check (list string)) "bob: his JOIN, the names" [ "JOIN"; "353"; "366" ] (commands (List.nth got 1));
          Alcotest.(check (list (pair string (list string)))) "the channel" [ ("#ocaml", [ "alice"; "bob" ]) ] (Irc_server.channels irc);
          say a "PRIVMSG #ocaml :hello, world";
          let got = pump irc [ a; b ] 50 in
          Alcotest.(check int) "not back to alice" 0 (List.length (List.nth got 0));
          (match List.nth got 1 with
          | [ m ] ->
              Alcotest.(check (option string)) "from alice" (Some "alice!alice@tiny") m.prefix;
              Alcotest.(check (list string)) "the line" [ "#ocaml"; "hello, world" ] m.params
          | _ -> Alcotest.fail "one line for bob");
          say b "PRIVMSG alice :just you";
          Alcotest.(check (list string)) "to a nick" [ "PRIVMSG" ] (commands (List.nth (pump irc [ a; b ] 50) 0));
          say b "PRIVMSG carol :hi";
          Alcotest.(check (list string)) "nobody" [ "401" ] (commands (List.nth (pump irc [ a; b ] 50) 1));
          say b "QUIT :bye";
          let got = pump irc [ a; b ] 50 in
          Alcotest.(check (list string)) "alice told" [ "QUIT" ] (commands (List.nth got 0));
          Alcotest.(check (list string)) "alone" [ "alice" ] (Irc_server.nicks irc));
    ]
