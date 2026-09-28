(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Civil.mli *)

type date = { year : int; month : int; day : int }

(* division rounding down, not towards zero: the years and days before
 * 1970 (and before year 0) are negative *)
let fdiv (a : int) (b : int) : int = if a >= 0 then a / b else -(((-a) + b - 1) / b)
let fmod (a : int) (b : int) : int = a - (b * fdiv a b)

let is_leap_year (y : int) : bool = (fmod y 4 = 0 && fmod y 100 <> 0) || fmod y 400 = 0

let days_in_month (y : int) (m : int) : int =
  match m with
  | 2 -> if is_leap_year y then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

let is_valid (d : date) : bool =
  d.month >= 1 && d.month <= 12 && d.day >= 1 && d.day <= days_in_month d.year d.month

(* 1970-01-01, counted from 0000-03-01 *)
let epoch_shift = 719468
let days_per_era = 146097

let days_from_civil (d : date) : int =
  (* the year starting in March: January and February belong to the
   * year before *)
  let y = if d.month <= 2 then d.year - 1 else d.year in
  let era = fdiv y 400 in
  let yoe = y - (era * 400) in                        (* [0, 399] *)
  let mp = if d.month > 2 then d.month - 3 else d.month + 9 in
  let doy = (((153 * mp) + 2) / 5) + d.day - 1 in     (* [0, 365] *)
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in  (* [0, 146096] *)
  (era * days_per_era) + doe - epoch_shift

let civil_from_days (z : int) : date =
  let z = z + epoch_shift in
  let era = fdiv z days_per_era in
  let doe = z - (era * days_per_era) in
  (* the year of the era: 365 days each, less the leap days so far
   * (one per 1460 days, but not per 36524, but per 146096) *)
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = if mp < 10 then mp + 3 else mp - 9 in
  let y = yoe + (era * 400) in
  { year = (if month <= 2 then y + 1 else y); month; day }

(* day 0 was a Thursday *)
let weekday (days : int) : int = fmod (days + 4) 7

let zeller (d : date) : int =
  let m, y = if d.month <= 2 then (d.month + 12, d.year - 1) else (d.month, d.year) in
  let k = fmod y 100 and j = fdiv y 100 in
  (* h: 0 for Saturday, as Zeller counted *)
  let h = fmod (d.day + (13 * (m + 1) / 5) + k + (k / 4) + fdiv j 4 + (5 * j)) 7 in
  fmod (h + 6) 7

let add_months (d : date) (n : int) : date =
  let m0 = (d.year * 12) + (d.month - 1) + n in
  let year = fdiv m0 12 and month = fmod m0 12 + 1 in
  { year; month; day = min d.day (days_in_month year month) }

let month_names =
  [| "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August";
     "September"; "October"; "November"; "December" |]

let weekday_names = [| "Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday" |]

let month_name (m : int) : string = month_names.(m - 1)
let weekday_name (w : int) : string = weekday_names.(w)

let to_string (d : date) : string = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day
