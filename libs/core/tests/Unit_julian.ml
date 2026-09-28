(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* core: Julian *)

let t = Testo.create

let date year month day : Civil.date = { year; month; day }

(* the worked examples of Julian.mli *)
let test_known () =
  Alcotest.(check int) "Julian 1582-10-04" (-141428) (Julian.days_from_julian (date 1582 10 4));
  Alcotest.(check int) "then Gregorian 1582-10-15" (-141427) (Civil.days_from_civil (date 1582 10 15));
  Alcotest.(check int) "Julian 1752-09-02, the day before England's switch" (Julian.england - 1)
    (Julian.days_from_julian (date 1752 9 2));
  Alcotest.(check int) "Julian day number of 2000-01-01" 2451545
    (Julian.julian_day_number (Civil.days_from_civil (date 2000 1 1)));
  Alcotest.(check string) "Julian day 0" "-4712-01-01" (Civil.to_string (Julian.julian_from_days (-2440588)));
  (* the calendars drift apart a day each century not divisible by 400 *)
  List.iter
    (fun (y, gap) ->
      Alcotest.(check int) (Printf.sprintf "the gap in %d" y) gap
        (Julian.days_from_julian (date y 3 1) - Civil.days_from_civil (date y 3 1)))
    [ (200, 0); (1582, 10); (1752, 11); (1900, 13); (2000, 13); (2100, 14) ];
  Alcotest.(check bool) "1900 is leap, for Julius" true (Julian.is_leap_year 1900)

(* cal 9 1752, and October 1582 in Rome *)
let test_switch () =
  let days switch y m = List.map (fun n -> (Julian.of_days switch n).day) (Julian.month switch y m) in
  Alcotest.(check (list int)) "September 1752 in England"
    ([ 1; 2 ] @ List.init 17 (fun i -> i + 14))
    (days Julian.england 1752 9);
  Alcotest.(check string) "its 1st, a Tuesday" "Tuesday"
    (Civil.weekday_name (Civil.weekday (List.hd (Julian.month Julian.england 1752 9))));
  Alcotest.(check (option int)) "1752-09-05 never was" None (Julian.to_days Julian.england (date 1752 9 5));
  Alcotest.(check (list int)) "October 1582 in Rome"
    (List.init 4 (fun i -> i + 1) @ List.init 17 (fun i -> i + 15))
    (days Julian.rome 1582 10);
  Alcotest.(check int) "September 1752 in Rome, a whole month" 30 (List.length (Julian.month Julian.rome 1752 9))

(* every Julian day from year 1 to 3000, there and back, each the day
 * after the one before *)
let test_every_day () =
  let first = Julian.days_from_julian (date 1 1 1) and last = Julian.days_from_julian (date 3000 12 31) in
  let prev = ref (Julian.julian_from_days (first - 1)) in
  for n = first to last do
    let d = Julian.julian_from_days n in
    if Julian.days_from_julian d <> n then Alcotest.failf "day %d: %s does not go back" n (Civil.to_string d);
    let p = !prev in
    let expected =
      if p.day < Julian.days_in_month p.year p.month then { p with day = p.day + 1 }
      else if p.month < 12 then date p.year (p.month + 1) 1
      else date (p.year + 1) 1 1
    in
    if d <> expected then Alcotest.failf "day %d: %s after %s" n (Civil.to_string d) (Civil.to_string p);
    prev := d
  done

let tests =
  Testo.categorize "Julian"
    [ t "the known days" test_known; t "the switch" test_switch; t "every day from 1 to 3000" test_every_day ]
