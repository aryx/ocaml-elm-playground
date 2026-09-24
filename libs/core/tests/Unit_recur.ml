(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* core: Recur (its rules as text are Ics's, tested in appkits/) *)

let t = Testo.create

let date year month day : Civil.date = { year; month; day }

let su = 0 and mo = 1 and tu = 2 and we = 3 and th = 4 and fr = 5

let rule ?(interval = 1) ?(by_day = []) ?(by_month_day = []) ?(week_start = mo) ?count ?until (freq : Recur.freq) :
    Recur.rule =
  { (Recur.make freq) with interval; by_day; by_month_day; week_start; count; until }

(* the occurrences as "MM-DD" (or the whole date when [years]) *)
let expand ?(years = false) ?(at = 9 * 3600) (r : Recur.rule) (start : Civil.date) ?(from = start) (upto : Civil.date) :
    string list =
  Recur.occurrences ~at r ~start ~from ~upto
  |> List.map (fun (d : Civil.date) ->
         if years then Civil.to_string d else Printf.sprintf "%02d-%02d" d.month d.day)

let check name expected actual = Alcotest.(check (list string)) name expected actual

(* RFC 5545, 3.8.5.3, the examples starting 1997-09-02 at 9:00; each
 * rule's text in the comment *)
let test_rfc () =
  let far = date 1999 1 1 in
  (* until midnight UTC on the 24th: the 24th's 9:00 is after it *)
  let christmas_eve = Recur.Until_time (date 1997 12 24, 0) in
  (* FREQ=DAILY;COUNT=10 *)
  check "daily for 10 occurrences" (List.init 10 (fun i -> Printf.sprintf "09-%02d" (i + 2)))
    (expand (rule Daily ~count:10) (date 1997 9 2) far);
  (* FREQ=DAILY;UNTIL=19971224T000000Z *)
  let daily = expand (rule Daily ~until:christmas_eve) (date 1997 9 2) far in
  Alcotest.(check int) "daily until December 24: how many" 113 (List.length daily);
  Alcotest.(check string) "and the last" "12-23" (List.nth daily 112);
  (* FREQ=DAILY;INTERVAL=2 *)
  check "every other day, in September" (List.init 15 (fun i -> Printf.sprintf "09-%02d" ((2 * i) + 2)))
    (expand (rule Daily ~interval:2) (date 1997 9 2) (date 1997 9 30));
  (* FREQ=WEEKLY;COUNT=10 *)
  check "weekly for 10 occurrences"
    [ "09-02"; "09-09"; "09-16"; "09-23"; "09-30"; "10-07"; "10-14"; "10-21"; "10-28"; "11-04" ]
    (expand (rule Weekly ~count:10) (date 1997 9 2) far);
  (* FREQ=WEEKLY;INTERVAL=2;WKST=SU;UNTIL=19971224T000000Z;BYDAY=MO,WE,FR *)
  check "every other week on Monday, Wednesday and Friday until December 24"
    [ "09-01"; "09-03"; "09-05"; "09-15"; "09-17"; "09-19"; "09-29"; "10-01"; "10-03"; "10-13"; "10-15";
      "10-17"; "10-27"; "10-29"; "10-31"; "11-10"; "11-12"; "11-14"; "11-24"; "11-26"; "11-28"; "12-08";
      "12-10"; "12-12"; "12-22" ]
    (expand (rule Weekly ~interval:2 ~week_start:su ~until:christmas_eve ~by_day:[ mo; we; fr ]) (date 1997 9 1) far);
  (* FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH *)
  check "every other week on Tuesday and Thursday, for 8 occurrences"
    [ "09-02"; "09-04"; "09-16"; "09-18"; "09-30"; "10-02"; "10-14"; "10-16" ]
    (expand (rule Weekly ~interval:2 ~count:8 ~week_start:su ~by_day:[ tu; th ]) (date 1997 9 2) far);
  (* FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15 *)
  check "the 2nd and 15th of the month, 10 occurrences"
    [ "1997-09-02"; "1997-09-15"; "1997-10-02"; "1997-10-15"; "1997-11-02"; "1997-11-15"; "1997-12-02";
      "1997-12-15"; "1998-01-02"; "1998-01-15" ]
    (expand ~years:true (rule Monthly ~count:10 ~by_month_day:[ 2; 15 ]) (date 1997 9 2) far);
  (* FREQ=MONTHLY;BYMONTHDAY=-3 *)
  check "the third-to-last day of the month"
    [ "1997-09-28"; "1997-10-29"; "1997-11-28"; "1997-12-29"; "1998-01-29"; "1998-02-26" ]
    (expand ~years:true (rule Monthly ~by_month_day:[ -3 ]) (date 1997 9 28) (date 1998 2 28))

(* the RFC's example of what the week's start changes: from Tuesday
 * 1997-08-05, every other week on Tuesday and Sunday
 * (FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO, then WKST=SU) *)
let test_week_start () =
  let r week_start = rule Weekly ~interval:2 ~count:4 ~by_day:[ tu; su ] ~week_start in
  check "weeks from Monday" [ "08-05"; "08-10"; "08-19"; "08-24" ] (expand (r mo) (date 1997 8 5) (date 1998 1 1));
  check "weeks from Sunday" [ "08-05"; "08-17"; "08-19"; "08-31" ] (expand (r su) (date 1997 8 5) (date 1998 1 1))

(* the days some months or years lack, and a window after the start *)
let test_missing_days () =
  check "the 31st: months without one skipped" [ "01-31"; "03-31"; "05-31"; "07-31"; "08-31" ]
    (expand (rule Monthly ~count:5) (date 2026 1 31) (date 2027 1 1));
  check "the 29th of February: leap years only" [ "2024-02-29"; "2028-02-29"; "2032-02-29" ]
    (expand ~years:true (rule Yearly ~count:3) (date 2024 2 29) (date 2040 1 1));
  check "a window later than the start, COUNT still from the start" [ "09-08"; "09-09"; "09-10"; "09-11" ]
    (expand (rule Daily ~count:10) (date 1997 9 2) ~from:(date 1997 9 8) (date 1997 12 31));
  check "a window past the end" [] (expand (rule Daily ~count:10) (date 1997 9 2) ~from:(date 1998 1 1) (date 1998 12 31))

let tests =
  Testo.categorize "Recur"
    [ t "RFC 5545's examples" test_rfc; t "the week's start" test_week_start; t "missing days, and windows" test_missing_days ]
