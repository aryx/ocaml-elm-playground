(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* core: Recur (and Ics's rules) *)

let t = Testo.create

let date year month day : Civil.date = { year; month; day }

let rule (s : string) : Recur.rule =
  match Ics.rule_of_string s with Some r -> r | None -> Alcotest.failf "%s not read" s

(* the occurrences as "MM-DD" (or the whole date when [years]) *)
let expand ?(years = false) ?(at = 9 * 3600) (s : string) (start : Civil.date) ?(from = start) (upto : Civil.date) :
    string list =
  Recur.occurrences ~at (rule s) ~start ~from ~upto
  |> List.map (fun (d : Civil.date) ->
         if years then Civil.to_string d else Printf.sprintf "%02d-%02d" d.month d.day)

let check name expected actual = Alcotest.(check (list string)) name expected actual

(* RFC 5545, 3.8.5.3, the examples starting 1997-09-02 at 9:00 *)
let test_rfc () =
  let far = date 1999 1 1 in
  check "daily for 10 occurrences" (List.init 10 (fun i -> Printf.sprintf "09-%02d" (i + 2)))
    (expand "FREQ=DAILY;COUNT=10" (date 1997 9 2) far);
  (* until midnight UTC on the 24th: the 24th's 9:00 is after it *)
  let daily = expand "FREQ=DAILY;UNTIL=19971224T000000Z" (date 1997 9 2) far in
  Alcotest.(check int) "daily until December 24: how many" 113 (List.length daily);
  Alcotest.(check string) "and the last" "12-23" (List.nth daily 112);
  check "every other day, in September" (List.init 15 (fun i -> Printf.sprintf "09-%02d" ((2 * i) + 2)))
    (expand "FREQ=DAILY;INTERVAL=2" (date 1997 9 2) (date 1997 9 30));
  check "weekly for 10 occurrences"
    [ "09-02"; "09-09"; "09-16"; "09-23"; "09-30"; "10-07"; "10-14"; "10-21"; "10-28"; "11-04" ]
    (expand "FREQ=WEEKLY;COUNT=10" (date 1997 9 2) far);
  check "every other week on Monday, Wednesday and Friday until December 24"
    [ "09-01"; "09-03"; "09-05"; "09-15"; "09-17"; "09-19"; "09-29"; "10-01"; "10-03"; "10-13"; "10-15";
      "10-17"; "10-27"; "10-29"; "10-31"; "11-10"; "11-12"; "11-14"; "11-24"; "11-26"; "11-28"; "12-08";
      "12-10"; "12-12"; "12-22" ]
    (expand "FREQ=WEEKLY;INTERVAL=2;WKST=SU;UNTIL=19971224T000000Z;BYDAY=MO,WE,FR" (date 1997 9 1) far);
  check "every other week on Tuesday and Thursday, for 8 occurrences"
    [ "09-02"; "09-04"; "09-16"; "09-18"; "09-30"; "10-02"; "10-14"; "10-16" ]
    (expand "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH" (date 1997 9 2) far);
  check "the 2nd and 15th of the month, 10 occurrences"
    [ "1997-09-02"; "1997-09-15"; "1997-10-02"; "1997-10-15"; "1997-11-02"; "1997-11-15"; "1997-12-02";
      "1997-12-15"; "1998-01-02"; "1998-01-15" ]
    (expand ~years:true "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15" (date 1997 9 2) far);
  check "the third-to-last day of the month"
    [ "1997-09-28"; "1997-10-29"; "1997-11-28"; "1997-12-29"; "1998-01-29"; "1998-02-26" ]
    (expand ~years:true "FREQ=MONTHLY;BYMONTHDAY=-3" (date 1997 9 28) (date 1998 2 28))

(* the RFC's example of what the week's start changes: from Tuesday
 * 1997-08-05, every other week on Tuesday and Sunday *)
let test_week_start () =
  check "weeks from Monday" [ "08-05"; "08-10"; "08-19"; "08-24" ]
    (expand "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO" (date 1997 8 5) (date 1998 1 1));
  check "weeks from Sunday" [ "08-05"; "08-17"; "08-19"; "08-31" ]
    (expand "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU" (date 1997 8 5) (date 1998 1 1))

(* the days some months or years lack, and a window after the start *)
let test_missing_days () =
  check "the 31st: months without one skipped" [ "01-31"; "03-31"; "05-31"; "07-31"; "08-31" ]
    (expand "FREQ=MONTHLY;COUNT=5" (date 2026 1 31) (date 2027 1 1));
  check "the 29th of February: leap years only" [ "2024-02-29"; "2028-02-29"; "2032-02-29" ]
    (expand ~years:true "FREQ=YEARLY;COUNT=3" (date 2024 2 29) (date 2040 1 1));
  check "a window later than the start, COUNT still from the start" [ "09-08"; "09-09"; "09-10"; "09-11" ]
    (expand "FREQ=DAILY;COUNT=10" (date 1997 9 2) ~from:(date 1997 9 8) (date 1997 12 31));
  check "a window past the end" [] (expand "FREQ=DAILY;COUNT=10" (date 1997 9 2) ~from:(date 1998 1 1) (date 1998 12 31))

(* what Recur leaves out is refused, not misread; and the text there and
 * back *)
let test_strings () =
  List.iter
    (fun s -> Alcotest.(check bool) (s ^ " refused") true (Ics.rule_of_string s = None))
    [ "FREQ=MONTHLY;BYDAY=1FR"; "FREQ=YEARLY;BYMONTH=1"; "FREQ=HOURLY"; "INTERVAL=2"; "FREQ=WEEKLY;BYMONTHDAY=3";
      "FREQ=MONTHLY;BYMONTHDAY=0"; "FREQ=DAILY;COUNT=many" ];
  List.iter
    (fun s -> Alcotest.(check string) s s (Ics.rule_to_string (rule s)))
    [ "FREQ=DAILY"; "FREQ=WEEKLY;INTERVAL=2;WKST=SU;BYDAY=MO,WE,FR;UNTIL=19971224T000000";
      "FREQ=MONTHLY;BYMONTHDAY=2,15,-1;COUNT=10"; "FREQ=YEARLY;UNTIL=20300101" ]

let tests =
  Testo.categorize "Recur"
    [ t "RFC 5545's examples" test_rfc;
      t "the week's start" test_week_start;
      t "missing days, and windows" test_missing_days;
      t "as text" test_strings ]
