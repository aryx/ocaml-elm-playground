(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_irc.mli *)

let ok = function Ok x -> x | Error e -> Alcotest.fail e

let tests =
  Testo.categorize "Irc"
    [
      Testo.create "the worked example: a PRIVMSG, there and back" (fun () ->
          let line = ":alice!alice@tiny PRIVMSG #ocaml :hello, world" in
          let m = ok (Irc.parse (line ^ "\r\n")) in
          Alcotest.(check (option string)) "prefix" (Some "alice!alice@tiny") m.prefix;
          Alcotest.(check string) "command" "PRIVMSG" m.command;
          Alcotest.(check (list string)) "params" [ "#ocaml"; "hello, world" ] m.params;
          Alcotest.(check string) "printed back" line (Irc.print m);
          Alcotest.(check string) "the nick" "alice" (Irc.nick_of (Option.get m.prefix)));
      Testo.create "PING, a numeric, the trailing only when needed" (fun () ->
          Alcotest.(check (list string)) "PING" [ "tiny" ] (ok (Irc.parse "PING :tiny")).params;
          Alcotest.(check string) "001" "001" (ok (Irc.parse ":tiny 001 alice :Welcome")).command;
          Alcotest.(check string) "no spaces: no colon" "JOIN #ocaml" (Irc.print (Irc.msg "JOIN" [ "#ocaml" ]));
          Alcotest.(check string) "empty: a colon" "PART #ocaml :" (Irc.print (Irc.msg "PART" [ "#ocaml"; "" ]));
          Alcotest.(check string) "lower case command" "NICK" (ok (Irc.parse "nick bob")).command);
      Testo.create "refused: no command, too long" (fun () ->
          Alcotest.(check bool) "empty" true (Result.is_error (Irc.parse ""));
          Alcotest.(check bool) "a prefix alone" true (Result.is_error (Irc.parse ":alice"));
          Alcotest.(check bool) "600 bytes" true (Result.is_error (Irc.parse ("PRIVMSG #a :" ^ String.make 600 'x'))));
    ]
