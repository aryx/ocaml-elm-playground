(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pim_date_book.mli *)

(* a row selected: an hour, or an event of no hour *)
type slot = Hour of int | Untimed of string

type t = { day : int; slot : slot option }

let start (today : int) : t = { day = today; slot = None }

let first_hour = 8
let y0 = 15.
let rows = 12

(* the day's events: those of no hour first, then by time *)
let of_day (d : Palm.data) (day : int) : Ics.event list =
  let date = Civil.civil_from_days day in
  List.filter (fun e -> Ics.occurrences e ~from:date ~upto:date <> []) d.events
  |> List.stable_sort (fun (a : Ics.event) (b : Ics.event) ->
         compare (Option.value ~default:(-1) a.start.time) (Option.value ~default:(-1) b.start.time))

let untimed (evs : Ics.event list) = List.filter (fun (e : Ics.event) -> e.start.time = None) evs

(* the rows: the events of no hour, then an hour each *)
let lines (evs : Ics.event list) : slot list =
  let u = untimed evs |> List.map (fun (e : Ics.event) -> Untimed e.uid) in
  u @ List.init (max 0 (rows - List.length u)) (fun i -> Hour (first_hour + i))

let at_hour (evs : Ics.event list) (h : int) : Ics.event option =
  List.find_opt (fun (e : Ics.event) -> match e.start.time with Some s -> s / 3600 = h | None -> false) evs

let week_x (i : int) = 88. +. (float_of_int i *. 10.)

let buttons = Palm.buttons [ "Today"; "<"; ">" ]

(* the selected row's event written into: a letter more or less, gone
 * when it has none left *)
let write (i : Palm.input) (d : Palm.data) (t : t) : Palm.data =
  let evs = of_day d t.day in
  let target = match t.slot with Some (Untimed uid) -> List.find_opt (fun (e : Ics.event) -> e.uid = uid) evs | Some (Hour h) -> at_hour evs h | None -> None in
  let edit (s : string) =
    let s = s ^ i.typed in
    if i.backspace && s <> "" then String.sub s 0 (String.length s - 1) else s
  in
  match (target, t.slot) with
  | _, None -> d
  | _ when i.typed = "" && not i.backspace -> d
  | Some e, _ ->
      let summary = edit e.summary in
      { d with
        events =
          (if summary = "" then List.filter (fun (x : Ics.event) -> x.uid <> e.uid) d.events
           else List.map (fun (x : Ics.event) -> if x.uid = e.uid then { x with summary } else x) d.events) }
  | None, Some (Hour h) when i.typed <> "" ->
      let uid, d = Palm.uid d in
      let at h : Ics.moment = { date = Civil.civil_from_days t.day; time = Some (h * 3600); utc = false } in
      let e : Ics.event =
        { uid; summary = i.typed; description = ""; location = ""; start = at h; end_ = Some (at (h + 1)); rrule = None }
      in
      { d with events = d.events @ [ e ] }
  | None, _ -> d

let update (i : Palm.input) (d : Palm.data) (t : t) : Palm.data * t =
  let t =
    match Palm.tapped i buttons with
    | Some "Today" -> { day = i.today; slot = None }
    | Some "<" -> { day = t.day - 1; slot = None }
    | Some ">" -> { day = t.day + 1; slot = None }
    | _ -> (
        match i.tap with
        (* a day of the week, in the title bar *)
        | Some (x, y) when y < 12. && x >= week_x 0 -. 5. ->
            let k = int_of_float ((x -. week_x 0 +. 5.) /. 10.) in
            if k < 7 then { day = t.day - Civil.weekday t.day + k; slot = None } else t
        | _ -> (
            match Palm.row_at ~y0 ~rows i with
            | Some r -> { t with slot = List.nth_opt (lines (of_day d t.day)) r }
            | None -> t))
  in
  let t = if i.up then { day = t.day - 1; slot = None } else if i.down then { day = t.day + 1; slot = None } else t in
  let t = if i.enter then { t with slot = None } else t in
  (write i d t, t)

let view ~(time : float) (d : Palm.data) (t : t) : Playground.shape list =
  let date = Civil.civil_from_days t.day in
  let heading = Printf.sprintf "%s %d, %02d" (String.sub (Civil.month_name date.month) 0 3) date.day (date.year mod 100) in
  let evs = of_day d t.day in
  let week =
    List.init 7 (fun k ->
        let letter = String.sub (Civil.weekday_name k) 0 1 in
        let x = week_x k in
        if k = Civil.weekday t.day then [ Palm.rect Palm.ink (x -. 2., 0., 9., 11.); Palm.text ~color:Palm.paper ~x ~y:0. letter ]
        else [ Palm.text ~x ~y:0. letter ])
    |> List.concat
  in
  let row r (s : slot) =
    let y = y0 +. (float_of_int r *. 11.) in
    let chosen = t.slot = Some s in
    let label, ev =
      match s with
      | Untimed uid -> ("*", List.find_opt (fun (e : Ics.event) -> e.uid = uid) evs)
      | Hour h -> (
          match at_hour evs h with
          | Some ({ start = { time = Some s; _ }; _ } as e) -> (Printf.sprintf "%d:%02d" (s / 3600) (s / 60 mod 60), Some e)
          | _ -> (Printf.sprintf "%d:00" h, None))
    in
    let summary = match ev with Some e -> e.summary | None -> "" in
    (if chosen then [ Palm.rect Palm.light (0., y, Palm.size, 11.) ] else [])
    @ [ Palm.text ~color:Palm.mid ~x:2. ~y label; Palm.text ~x:32. ~y (Palm.fit 124. summary);
        Palm.rect Palm.light (32., y +. 10., 124., 0.5) ]
    @ if chosen then Palm.caret ~time ~x:32. ~y summary else []
  in
  Palm.title heading @ week @ List.concat (List.mapi row (lines evs)) @ Palm.draw_buttons buttons
