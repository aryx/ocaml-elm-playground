(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/pim: Ics *)

let t = Testo.create

let date year month day : Civil.date = { year; month; day }

(* a long line cut at 75 bytes, and back; a UTF-8 character never cut *)
let test_fold () =
  let check_folded line =
    let folded = Ics.fold line in
    let physical = String.split_on_char '\n' folded in
    List.iteri
      (fun i l ->
        let l = if String.ends_with ~suffix:"\r" l then String.sub l 0 (String.length l - 1) else l in
        if String.length l > 75 then Alcotest.failf "line %d: %d bytes" i (String.length l);
        (* after the continuation's space, never a byte inside a character *)
        if i > 0 && String.length l > 1 && Char.code l.[1] land 0xC0 = 0x80 then Alcotest.failf "line %d cut a character" i)
      physical;
    Alcotest.(check (list string)) "unfolded back" [ line ] (Ics.unfold folded)
  in
  check_folded ("DESCRIPTION:" ^ String.make 200 'x');
  check_folded ("SUMMARY:" ^ String.concat "" (List.init 100 (fun _ -> "\xc3\xa9")) (* é *));
  Alcotest.(check string) "a short line untouched" "SUMMARY:hi" (Ics.fold "SUMMARY:hi");
  Alcotest.(check (list string)) "LF alone, a tab continuing, empty lines" [ "A:1"; "B:23" ] (Ics.unfold "A:1\n\nB:2\n\t3\n")

let test_content_lines () =
  let check s expected =
    Alcotest.(check (option (triple string (list (pair string string)) string))) s expected (Ics.content_line s)
  in
  check "DTSTART;VALUE=DATE:20261001" (Some ("DTSTART", [ ("VALUE", "DATE") ], "20261001"));
  check "summary:lower case name" (Some ("SUMMARY", [], "lower case name"));
  (* a quoted parameter holds ';' and ':' *)
  check "ATTENDEE;CN=\"Doe; John: Jr\";ROLE=CHAIR:mailto:jd@example.com"
    (Some ("ATTENDEE", [ ("CN", "Doe; John: Jr"); ("ROLE", "CHAIR") ], "mailto:jd@example.com"));
  check "no colon" None;
  check "TEL;HOME;VOICE:555" (Some ("TEL", [ ("TYPE", "HOME"); ("TYPE", "VOICE") ], "555"));
  let text = "one, two; three\\four\nfive" in
  Alcotest.(check string) "escaped" "one\\, two\\; three\\\\four\\nfive" (Ics.escape text);
  Alcotest.(check string) "and back" text (Ics.unescape (Ics.escape text));
  Alcotest.(check string) "\\N too" "a\nb" (Ics.unescape "a\\Nb")

let test_moments () =
  let check s expected =
    let show (m : Ics.moment) =
      Printf.sprintf "%s %s%s" (Civil.to_string m.date)
        (match m.time with Some t -> string_of_int t | None -> "-")
        (if m.utc then " utc" else "")
    in
    Alcotest.(check (option string)) s expected (Option.map show (Ics.moment_of_string s));
    Option.iter (fun m -> Alcotest.(check string) (s ^ " back") s (Ics.moment_to_string m)) (Ics.moment_of_string s)
  in
  check "20260928" (Some "2026-09-28 -");
  check "20260928T090000" (Some "2026-09-28 32400");
  check "19960918T143000Z" (Some "1996-09-18 52200 utc");
  check "20260230" None;
  check "20260928T250000" None;
  check "2026-09-28" None

(* RRULE's text: read into Recur's rule, what Recur leaves out refused
 * rather than misread, and written back the same *)
let test_rules () =
  Alcotest.(check bool) "the RFC's every-other-week rule, read" true
    (Ics.rule_of_string "FREQ=WEEKLY;INTERVAL=2;WKST=SU;UNTIL=19971224T000000Z;BYDAY=MO,WE,FR"
    = Some
        { (Recur.make Weekly) with
          interval = 2; week_start = 0; until = Some (Until_time (date 1997 12 24, 0)); by_day = [ 1; 3; 5 ] });
  List.iter
    (fun s -> Alcotest.(check bool) (s ^ " refused") true (Ics.rule_of_string s = None))
    [ "FREQ=MONTHLY;BYDAY=1FR"; "FREQ=YEARLY;BYMONTH=1"; "FREQ=HOURLY"; "INTERVAL=2"; "FREQ=WEEKLY;BYMONTHDAY=3";
      "FREQ=MONTHLY;BYMONTHDAY=0"; "FREQ=DAILY;COUNT=many" ];
  List.iter
    (fun s ->
      match Ics.rule_of_string s with
      | Some r -> Alcotest.(check string) s s (Ics.rule_to_string r)
      | None -> Alcotest.failf "%s not read" s)
    [ "FREQ=DAILY"; "FREQ=WEEKLY;INTERVAL=2;WKST=SU;BYDAY=MO,WE,FR;UNTIL=19971224T000000";
      "FREQ=MONTHLY;BYMONTHDAY=2,15,-1;COUNT=10"; "FREQ=YEARLY;UNTIL=20300101" ]

(* RFC 5545, section 4, its first example: folded over three lines,
 * with an escaped comma and newlines *)
let rfc_example =
  String.concat "\r\n"
    [ "BEGIN:VCALENDAR"; "PRODID:-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN"; "VERSION:2.0"; "BEGIN:VEVENT";
      "DTSTAMP:19960704T120000Z"; "UID:uid1@example.com"; "ORGANIZER:mailto:jsmith@example.com";
      "DTSTART:19960918T143000Z"; "DTEND:19960920T220000Z"; "STATUS:CONFIRMED"; "CATEGORIES:CONFERENCE";
      "SUMMARY:Networld+Interop Conference"; "DESCRIPTION:Networld+Interop Conference";
      "  and Exhibit\\nAtlanta World Congress Center\\n"; " Atlanta\\, Georgia"; "END:VEVENT"; "END:VCALENDAR"; "" ]

let test_rfc () =
  let cal = Ics.of_string rfc_example in
  match cal.events with
  | [ e ] ->
      Alcotest.(check string) "uid" "uid1@example.com" e.uid;
      Alcotest.(check string) "summary" "Networld+Interop Conference" e.summary;
      Alcotest.(check string) "description, unfolded and unescaped"
        "Networld+Interop Conference and Exhibit\nAtlanta World Congress Center\nAtlanta, Georgia" e.description;
      Alcotest.(check string) "start" "19960918T143000Z" (Ics.moment_to_string e.start);
      Alcotest.(check (option string)) "end" (Some "19960920T220000Z") (Option.map Ics.moment_to_string e.end_)
  | es -> Alcotest.failf "%d events" (List.length es)

(* what a calendar program exports: a time zone and an alarm to skip,
 * an event without a start to drop, a rule we don't follow *)
let test_lenient () =
  let text =
    String.concat "\n"
      [ "BEGIN:VCALENDAR"; "BEGIN:VTIMEZONE"; "TZID:Europe/Paris"; "BEGIN:STANDARD"; "DTSTART:19701025T030000";
        "END:STANDARD"; "END:VTIMEZONE"; "BEGIN:VEVENT"; "UID:a"; "DTSTART;TZID=Europe/Paris:20260928T090000";
        "RRULE:FREQ=WEEKLY;BYDAY=MO"; "SUMMARY:Standup"; "BEGIN:VALARM"; "ACTION:DISPLAY"; "SUMMARY:not this one";
        "END:VALARM"; "END:VEVENT"; "BEGIN:VEVENT"; "UID:b"; "SUMMARY:no start"; "END:VEVENT"; "BEGIN:VEVENT";
        "UID:c"; "DTSTART;VALUE=DATE:20261225"; "RRULE:FREQ=MONTHLY;BYDAY=1FR"; "SUMMARY:first Fridays";
        "END:VEVENT"; "BEGIN:VTODO"; "UID:d"; "SUMMARY:done"; "STATUS:COMPLETED"; "PRIORITY:2"; "END:VTODO";
        "END:VCALENDAR" ]
  in
  let cal = Ics.of_string text in
  Alcotest.(check (list string)) "the events kept" [ "Standup"; "first Fridays" ]
    (List.map (fun (e : Ics.event) -> e.summary) cal.events);
  Alcotest.(check bool) "the TZID read as floating time" false (List.hd cal.events).start.utc;
  Alcotest.(check bool) "a weekly rule" true ((List.hd cal.events).rrule <> None);
  Alcotest.(check bool) "an ordinal BYDAY, no rule" true ((List.nth cal.events 1).rrule = None);
  Alcotest.(check (list (pair string (pair int bool)))) "the to-do" [ ("done", (2, true)) ]
    (List.map (fun (t : Ics.todo) -> (t.summary, (t.priority, t.completed))) cal.todos);
  Alcotest.(check int) "garbage: nothing, no exception" 0 (List.length (Ics.of_string "\x00BEGIN;;;:\n:::").events)

(* written, then read back the same *)
let test_round_trip () =
  let cal : Ics.calendar =
    { events =
        [ { uid = "1@tiny"; summary = "Standup, then coffee"; description = "Room 4;\nbring a mug"; location = "";
            start = { date = date 2026 9 28; time = Some (9 * 3600); utc = false };
            end_ = Some { date = date 2026 9 28; time = Some (10 * 3600); utc = false };
            rrule = Ics.rule_of_string "FREQ=WEEKLY;BYDAY=MO,TH" };
          { uid = "2@tiny"; summary = String.concat " " (List.init 30 (fun i -> "word" ^ string_of_int i));
            description = ""; location = "Everywhere"; start = { date = date 2026 12 25; time = None; utc = false };
            end_ = None; rrule = Ics.rule_of_string "FREQ=YEARLY" } ];
      todos =
        [ { uid = "3@tiny"; summary = "Write the plan"; due = Some { date = date 2026 10 1; time = None; utc = false };
            priority = 1; completed = false };
          { uid = "4@tiny"; summary = "Read RFC 5545"; due = None; priority = 0; completed = true } ] }
  in
  let text = Ics.to_string ~stamp:{ date = date 2026 9 24; time = Some 43200; utc = true } cal in
  Alcotest.(check bool) "CRLF line ends" true (String.ends_with ~suffix:"END:VCALENDAR\r\n" text);
  Alcotest.(check bool) "a DTSTAMP" true (Ics.unfold text |> List.mem "DTSTAMP:20260924T120000Z");
  Alcotest.(check bool) "a date as VALUE=DATE" true (Ics.unfold text |> List.mem "DUE;VALUE=DATE:20261001");
  Alcotest.(check bool) "read back the same" true (Ics.of_string text = cal)

let tests =
  Testo.categorize "Ics"
    [ t "folding" test_fold;
      t "content lines and escapes" test_content_lines;
      t "dates and times" test_moments;
      t "rules as text" test_rules;
      t "RFC 5545's first example" test_rfc;
      t "lenient reading" test_lenient;
      t "written and read back" test_round_trip ]
