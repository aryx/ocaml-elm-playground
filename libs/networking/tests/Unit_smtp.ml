(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_smtp.mli *)

let lines = Alcotest.(check (list string))

(* the server's lines fed to the client, one at a time; what it sent *)
let replay (c : Smtp.client) (server : string list) : Smtp.client * string list =
  List.fold_left (fun (c, sent) l -> let c, out = Smtp.step c l in (c, sent @ out)) (c, []) server

let outcome = Alcotest.testable (fun ppf o -> Format.pp_print_string ppf (match o with Smtp.Sent (n, r) -> Printf.sprintf "Sent %d [%s]" n (String.concat ";" r) | Smtp.Refused w -> "Refused " ^ w)) ( = )

let rfc = { Smtp.sender = "Smith@bar.com"; recipients = [ "Jones@foo.com"; "Green@foo.com"; "Brown@foo.com" ]; text = "Blah blah blah...\n...etc. etc. etc.\n" }

let tests =
  Testo.categorize "Smtp"
    [
      Testo.create "RFC 5321's session, D.1: a recipient refused, the others kept" (fun () ->
          let c, sent =
            replay (Smtp.client ~hello:"bar.com" [ rfc ])
              [ "220 foo.com Simple Mail Transfer Service Ready"; "250-foo.com greets bar.com"; "250-8BITMIME"; "250-SIZE"; "250-DSN"; "250 HELP";
                "250 OK"; "250 OK"; "550 No such user here"; "250 OK"; "354 Start mail input; end with <CRLF>.<CRLF>"; "250 OK";
                "221 foo.com Service closing transmission channel" ]
          in
          (* the RFC's "...etc. etc. etc." stands for more of the message;
             sent, a line starting with a dot gets one more *)
          lines "what the client said"
            [ "EHLO bar.com"; "MAIL FROM:<Smith@bar.com>"; "RCPT TO:<Jones@foo.com>"; "RCPT TO:<Green@foo.com>"; "RCPT TO:<Brown@foo.com>"; "DATA";
              "Blah blah blah..."; "....etc. etc. etc."; "."; "QUIT" ]
            sent;
          Alcotest.(check (option (result (list outcome) string))) "sent to two" (Some (Ok [ Smtp.Sent (2, [ "550 No such user here" ]) ])) (Smtp.finished c));
      Testo.create "a server of 1982: EHLO refused, HELO" (fun () ->
          let _, sent = replay (Smtp.client ~hello:"eudora" []) [ "220 tiny"; "500 what?"; "250 tiny"; "221 bye" ] in
          lines "HELO after" [ "EHLO eudora"; "HELO eudora"; "QUIT" ] sent);
      Testo.create "the dot: stuffed, and taken off" (fun () ->
          lines "stuffed" [ "a"; "..b"; ".."; "." ] (Smtp.stuff "a\n.b\n.\n");
          Alcotest.(check string) "unstuffed" "a\n.b\n.\n" (Smtp.unstuff (Smtp.stuff "a\n.b\n.\n")));
      Testo.create "Bcc: in the envelope, not in the message" (fun () ->
          let m = Mail.make [ ("From", "bob@tiny"); ("To", "Alice <alice@tiny>"); ("Cc", "carol@tiny"); ("Bcc", "dave@tiny") ] "hi\n" in
          let e = Smtp.envelope ~sender:"bob@tiny" m in
          lines "recipients" [ "alice@tiny"; "carol@tiny"; "dave@tiny" ] e.recipients;
          Alcotest.(check bool) "no Bcc: sent" false (Option.is_some (Mail.get (Mail.parse e.text) "bcc")));
      Testo.create "a message refused whole, RSET, the next one sent" (fun () ->
          let one = { rfc with recipients = [ "nobody@elsewhere" ] } and two = { rfc with recipients = [ "Jones@foo.com" ] } in
          let c, sent = replay (Smtp.client ~hello:"bar.com" [ one; two ]) [ "220 x"; "250 x"; "250 OK"; "550 not a relay"; "250 reset"; "250 OK"; "250 OK"; "354 go"; "250 OK"; "221 bye" ] in
          lines "RSET between" [ "EHLO bar.com"; "MAIL FROM:<Smith@bar.com>"; "RCPT TO:<nobody@elsewhere>"; "RSET"; "MAIL FROM:<Smith@bar.com>"; "RCPT TO:<Jones@foo.com>"; "DATA"; "Blah blah blah..."; "....etc. etc. etc."; "."; "QUIT" ] sent;
          Alcotest.(check (option (result (list outcome) string))) "one refused, one sent" (Some (Ok [ Smtp.Refused "no recipient accepted"; Smtp.Sent (1, []) ])) (Smtp.finished c));
      Testo.create "no greeting: the connection given up" (fun () ->
          let c, _ = replay (Smtp.client ~hello:"x" [ rfc ]) [ "554 go away" ] in
          Alcotest.(check bool) "an error" true (match Smtp.finished c with Some (Error _) -> true | _ -> false));
      Testo.create "AUTH PLAIN: RFC 4616's example, and a session through it" (fun () ->
          Alcotest.(check string) "tim" "AHRpbQB0YW5zdGFhZnRhbnN0YWFm" (Smtp.plain ~user:"tim" ~password:"tanstaaftanstaaf");
          let c, sent =
            replay (Smtp.client ~hello:"eudora" ~auth:("tim", "tanstaaftanstaaf") [ { rfc with recipients = [ "Jones@foo.com" ] } ])
              [ "220 smtp"; "250-smtp at your service"; "250 AUTH LOGIN PLAIN"; "235 2.7.0 Accepted"; "250 OK"; "250 OK"; "354 go"; "250 OK"; "221 bye" ]
          in
          lines "AUTH after EHLO" [ "EHLO eudora"; "AUTH PLAIN AHRpbQB0YW5zdGFhZnRhbnN0YWFm"; "MAIL FROM:<Smith@bar.com>"; "RCPT TO:<Jones@foo.com>"; "DATA" ] (List.filteri (fun i _ -> i < 5) sent);
          Alcotest.(check (option (result (list outcome) string))) "sent" (Some (Ok [ Smtp.Sent (1, []) ])) (Smtp.finished c);
          let c, _ = replay (Smtp.client ~hello:"eudora" ~auth:("tim", "wrong") [ rfc ]) [ "220 smtp"; "250 smtp"; "535 5.7.8 Username and Password not accepted" ] in
          Alcotest.(check bool) "refused: given up" true (match Smtp.finished c with Some (Error _) -> true | _ -> false));
      Testo.create "commands parsed, in any case" (fun () ->
          Alcotest.(check bool) "mail" true (Smtp.parse_command "mail from: <a@b>" = Smtp.Mail_from "a@b");
          lines "a reply of two lines" [ "250-tiny"; "250 HELP" ] (Smtp.reply 250 [ "tiny"; "HELP" ]));
    ]
