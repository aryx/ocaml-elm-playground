(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_mail.mli *)

let example =
  "From: Alice <alice@tiny>\n\
   To: bob@tiny\n\
   Subject: lunch\n\
   Date: Fri, 25 Sep 2026 12:00:00 +0200\n\
   Message-ID: <1@tiny>\n\
   Received: from eudora by tiny;\n\
  \  Fri, 25 Sep 2026 12:00:01 +0200\n\
   \n\
   Noon at the usual place?\n"

let str = Alcotest.(check string)
let opt = Alcotest.(check (option string))

let tests =
  Testo.categorize "Mail"
    [
      Testo.create "the worked example: fields, folding, there and back" (fun () ->
          let m = Mail.parse example in
          Alcotest.(check int) "six fields" 6 (List.length m.fields);
          opt "any case" (Some "lunch") (Mail.get m "subject");
          opt "unfolded" (Some "from eudora by tiny;  Fri, 25 Sep 2026 12:00:01 +0200") (Mail.get m "received");
          str "the body" "Noon at the usual place?\n" m.body;
          str "the same bytes" example (Mail.to_string m);
          str "CR LF read as LF" example (Mail.to_string (Mail.parse (String.concat "\r\n" (String.split_on_char '\n' example)))));
      Testo.create "the worked example: its address and its date" (fun () ->
          let m = Mail.parse example in
          let a = Option.get (Mail.address (Option.get (Mail.get m "from"))) in
          str "display" "Alice" a.display;
          str "mailbox" "alice@tiny" a.mailbox;
          let d = Option.get (Mail.date (Option.get (Mail.get m "date"))) in
          Alcotest.(check int) "offset" 120 d.offset;
          Alcotest.(check int) "hour" 12 d.time.hour;
          (* 10:00 in Greenwich: the same instant written with +0000 *)
          let utc = Option.get (Mail.date "25 Sep 2026 10:00 GMT") in
          Alcotest.(check (float 0.)) "the same instant" (Mail.seconds utc) (Mail.seconds d);
          str "written back" "Fri, 25 Sep 2026 12:00:00 +0200" (Mail.date_to_string d));
      Testo.create "addresses: lists, quotes, the old comment form" (fun () ->
          let l = Mail.addresses "\"Smith, J\" <j@tiny>, bob@tiny, carol@tiny (Carol)" in
          Alcotest.(check (list string)) "three" [ "Smith, J"; "bob@tiny"; "Carol" ] (List.map Mail.who l);
          str "quoted when it must be" "\"Smith, J\" <j@tiny>" (Mail.address_to_string (List.hd l));
          str "not otherwise" "Alice <alice@tiny>" (Mail.address_to_string { display = "Alice"; mailbox = "alice@tiny" }));
      Testo.create "dates: old zones, two-digit years, garbage" (fun () ->
          let d = Option.get (Mail.date "Mon, 3 Jul 95 09:41 PDT") in
          Alcotest.(check int) "1995" 1995 d.day.year;
          Alcotest.(check int) "PDT" (-420) d.offset;
          Alcotest.(check bool) "garbage" true (Mail.date "yesterday" = None));
      Testo.create "set, message ids" (fun () ->
          let m = Mail.set "Subject" "Re: lunch" (Mail.parse example) in
          opt "replaced" (Some "Re: lunch") (Mail.get m "subject");
          Alcotest.(check int) "still six" 6 (List.length m.fields);
          Alcotest.(check (list string)) "ids" [ "1@tiny"; "2@tiny" ] (Mail.message_ids "<1@tiny>\n <2@tiny>"));
      Testo.create "mbox: >From quoted and unquoted, mboxrd" (fun () ->
          let body = "From the desk of Alice\n>From the desk of Bob\n" in
          str "escaped" ">From the desk of Alice\n>>From the desk of Bob\n" (Mbox.escape body);
          str "and back" body (Mbox.unescape (Mbox.escape body));
          let d = Option.get (Mail.date "Fri, 25 Sep 2026 12:00:00 +0200") in
          let e = { Mbox.envelope = Mbox.envelope ~sender:"alice@tiny" d; mail = Mail.make [ ("Subject", "memo") ] body } in
          let file = Mbox.to_string [ e; e ] in
          match Mbox.parse file with
          | [ a; b ] ->
              str "the envelope" "alice@tiny Fri Sep 25 12:00:00 2026" a.envelope;
              str "sender" "alice@tiny" (Mbox.sender a);
              str "the body as it was" body b.mail.body;
              str "the same bytes" file (Mbox.to_string [ a; b ])
          | l -> Alcotest.failf "%d messages" (List.length l));
    ]
