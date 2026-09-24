(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Recur.mli *)

type freq = Daily | Weekly | Monthly | Yearly
type until = Until_date of Civil.date | Until_time of Civil.date * int

type rule = {
  freq : freq;
  interval : int;
  by_day : int list;
  by_month_day : int list;
  week_start : int;
  count : int option;
  until : until option;
}

let make (freq : freq) : rule =
  { freq; interval = 1; by_day = []; by_month_day = []; week_start = 1; count = None; until = None }

(* the [k]th period's first day, and the days picked in it, as day
 * numbers, in order *)
let period (r : rule) (start : Civil.date) (k : int) : int * int list =
  let s = Civil.days_from_civil start in
  let n = k * r.interval in
  match r.freq with
  | Daily -> (s + n, [ s + n ])
  | Weekly ->
      (* the week holding the start, from its [week_start] *)
      let first = s - (((Civil.weekday s - r.week_start) + 7) mod 7) + (7 * n) in
      let days = if r.by_day = [] then [ Civil.weekday s ] else r.by_day in
      (first, List.map (fun wd -> first + (((wd - r.week_start) + 7) mod 7)) days |> List.sort_uniq compare)
  | Monthly ->
      let m = Civil.add_months { start with day = 1 } n in
      let last = Civil.days_in_month m.year m.month in
      let days = if r.by_month_day = [] then [ start.day ] else r.by_month_day in
      let first = Civil.days_from_civil m in
      ( first,
        days
        |> List.map (fun d -> if d < 0 then last + 1 + d else d)
        |> List.filter (fun d -> d >= 1 && d <= last)
        |> List.map (fun d -> first + d - 1)
        |> List.sort_uniq compare )
  | Yearly ->
      let y = start.year + n in
      let d : Civil.date = { start with year = y } in
      (Civil.days_from_civil { year = y; month = 1; day = 1 }, if Civil.is_valid d then [ Civil.days_from_civil d ] else [])

let occurrences ?(at = 0) (r : rule) ~(start : Civil.date) ~(from : Civil.date) ~(upto : Civil.date) : Civil.date list =
  let s = Civil.days_from_civil start in
  let from = Civil.days_from_civil from and upto = Civil.days_from_civil upto in
  let within_until (n : int) =
    match r.until with
    | None -> true
    | Some (Until_date u) -> n <= Civil.days_from_civil u
    | Some (Until_time (u, t)) ->
        let u = Civil.days_from_civil u in
        n < u || (n = u && at <= t)
  in
  (* period by period; [seen]: the occurrences so far, for [count] *)
  let rec go k seen acc =
    let first, days = period r start k in
    if first > upto then List.rev acc
    else
      let rec pick days seen acc =
        match days with
        | [] -> `More (seen, acc)
        | n :: rest ->
            if n < s then pick rest seen acc
            else if (match r.count with Some c -> seen >= c | None -> false) || not (within_until n) || n > upto then
              `Done acc
            else pick rest (seen + 1) (if n >= from then n :: acc else acc)
      in
      match pick days seen acc with
      | `Done acc -> List.rev acc
      | `More (seen, acc) -> go (k + 1) seen acc
  in
  go 0 0 [] |> List.map Civil.civil_from_days
