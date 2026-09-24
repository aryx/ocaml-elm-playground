(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Julian.mli *)

let fdiv (a : int) (b : int) : int = if a >= 0 then a / b else -(((-a) + b - 1) / b)
let fmod (a : int) (b : int) : int = a - (b * fdiv a b)

let is_leap_year (y : int) : bool = fmod y 4 = 0

let days_in_month (y : int) (m : int) : int =
  match m with
  | 2 -> if is_leap_year y then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

(* Julian 0000-03-01, counted from 1970-01-01: two days before the
 * Gregorian one (the calendars agreed in the 3rd century) *)
let epoch_shift = 719470
let days_per_era = 1461

(* as Civil's, the leap day last in a year starting in March, but
 * every 4th year has one: 365 days a year and the era's leap day at
 * its very end *)
let days_from_julian (d : Civil.date) : int =
  let y = if d.month <= 2 then d.year - 1 else d.year in
  let era = fdiv y 4 in
  let yoe = y - (era * 4) in
  let mp = if d.month > 2 then d.month - 3 else d.month + 9 in
  let doy = (((153 * mp) + 2) / 5) + d.day - 1 in
  (era * days_per_era) + (yoe * 365) + doy - epoch_shift

let julian_from_days (z : int) : Civil.date =
  let z = z + epoch_shift in
  let era = fdiv z days_per_era in
  let doe = z - (era * days_per_era) in
  (* 1460, the era's leap day, is still the 4th year's *)
  let yoe = (doe - (doe / 1460)) / 365 in
  let doy = doe - (365 * yoe) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = if mp < 10 then mp + 3 else mp - 9 in
  let y = yoe + (era * 4) in
  { year = (if month <= 2 then y + 1 else y); month; day }

type switch = int

let england = Civil.days_from_civil { year = 1752; month = 9; day = 14 }
let rome = Civil.days_from_civil { year = 1582; month = 10; day = 15 }

let of_days (switch : switch) (n : int) : Civil.date =
  if n < switch then julian_from_days n else Civil.civil_from_days n

let to_days (switch : switch) (d : Civil.date) : int option =
  (* the date read both ways; the one that names itself back is right *)
  let g = Civil.days_from_civil d and j = days_from_julian d in
  if g >= switch && Civil.is_valid d then Some g
  else if j < switch && d.month >= 1 && d.month <= 12 && d.day >= 1 && d.day <= days_in_month d.year d.month then Some j
  else None

let month (switch : switch) (year : int) (m : int) : int list =
  List.init 31 (fun i -> to_days switch { year; month = m; day = i + 1 }) |> List.filter_map Fun.id

(* JD 2440587.5 is 1970-01-01 at midnight; the number of that day starts
 * at the following noon *)
let julian_day_number (n : int) : int = n + 2440588
