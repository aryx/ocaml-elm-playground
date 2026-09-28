(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* core: Civil *)

let t = Testo.create

let date year month day : Civil.date = { year; month; day }

(* the worked example of Civil.mli, and the days everyone knows *)
let test_known () =
  List.iter
    (fun (d, days, weekday) ->
      let s = Civil.to_string d in
      Alcotest.(check int) (s ^ " day number") days (Civil.days_from_civil d);
      Alcotest.(check string) (s ^ " back") s (Civil.to_string (Civil.civil_from_days days));
      Alcotest.(check string) (s ^ " weekday") weekday (Civil.weekday_name (Civil.weekday days)))
    [ (date 1970 1 1, 0, "Thursday");
      (date 1969 12 31, -1, "Wednesday");
      (date 2000 3 1, 11017, "Wednesday");
      (date 2026 1 1, 20454, "Thursday");
      (date 2026 9 24, 20720, "Thursday");
      (* the first day of the Gregorian calendar, a Friday *)
      (date 1582 10 15, -141427, "Friday");
      (date 0 3 1, -719468, "Wednesday") ]

(* the leap-year rule, and the day it creates or not *)
let test_leap () =
  List.iter
    (fun (y, leap) -> Alcotest.(check bool) (string_of_int y) leap (Civil.is_leap_year y))
    [ (2000, true); (1900, false); (2024, true); (2026, false); (2100, false); (2400, true); (0, true); (-4, true) ];
  Alcotest.(check bool) "2000-02-29" true (Civil.is_valid (date 2000 2 29));
  Alcotest.(check bool) "1900-02-29" false (Civil.is_valid (date 1900 2 29));
  Alcotest.(check bool) "2026-04-31" false (Civil.is_valid (date 2026 4 31));
  Alcotest.(check bool) "2026-13-01" false (Civil.is_valid (date 2026 13 1))

(* every day of 800 years (two eras), there and back, one after the
 * other, and Zeller agreeing with the day number *)
let test_every_day () =
  let first = Civil.days_from_civil (date 1600 1 1) and last = Civil.days_from_civil (date 2400 12 31) in
  let prev = ref (Civil.civil_from_days (first - 1)) in
  for n = first to last do
    let d = Civil.civil_from_days n in
    if not (Civil.is_valid d) then Alcotest.failf "day %d: %s is not a date" n (Civil.to_string d);
    if Civil.days_from_civil d <> n then Alcotest.failf "day %d: %s does not go back" n (Civil.to_string d);
    (* the next day: the day after, or the 1st of the next month *)
    let p = !prev in
    let expected =
      if p.day < Civil.days_in_month p.year p.month then { p with day = p.day + 1 }
      else if p.month < 12 then date p.year (p.month + 1) 1
      else date (p.year + 1) 1 1
    in
    if d <> expected then Alcotest.failf "day %d: %s after %s" n (Civil.to_string d) (Civil.to_string p);
    if Civil.zeller d <> Civil.weekday n then Alcotest.failf "%s: Zeller disagrees" (Civil.to_string d);
    prev := d
  done;
  (* 800 years, two eras of 146097 days *)
  Alcotest.(check int) "two eras" (2 * 146097) (Civil.days_from_civil (date 2400 1 1) - Civil.days_from_civil (date 1600 1 1))

let test_add_months () =
  List.iter
    (fun (d, n, expected) ->
      Alcotest.(check string)
        (Printf.sprintf "%s %+d" (Civil.to_string d) n)
        expected (Civil.to_string (Civil.add_months d n)))
    [ (date 2024 1 31, 1, "2024-02-29");
      (date 2023 1 31, 1, "2023-02-28");
      (date 2026 12 15, 1, "2027-01-15");
      (date 2026 1 15, -1, "2025-12-15");
      (date 2026 9 24, -24, "2024-09-24");
      (date 2026 3 31, 0, "2026-03-31") ]

let tests =
  Testo.categorize "Civil"
    [ t "the known days" test_known;
      t "leap years" test_leap;
      t "every day from 1600 to 2400" test_every_day;
      t "months added" test_add_months ]
