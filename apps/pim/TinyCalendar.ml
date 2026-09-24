(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of a calendar program, from Unix's cal (1971), a month
 * printed as a grid of weeks, to iCal (Apple, 2002), a week of hours
 * with events you drag around, which repeat, and which any other
 * calendar can read.
 *
 *   click a day (or an hour, in the week)   select it
 *   type a title, Enter or Add              an event there (all day
 *                                           in the month, an hour in
 *                                           the week)
 *   drag an event                           to another day or hour
 *   drag an event's bottom edge (week)      to make it longer
 *   View                                    month or week; which
 *                                           calendar the past is in
 *   Repeat                                  the selected event's rule
 *   File, Import, Export .ics               saved; read from and
 *                                           written as iCalendar
 *
 * flags date=1752-09-02 (the day shown, as people wrote it then),
 * view=week, switch=rome or switch=none.
 *
 * What it teaches is in core's time/ and the pim appkit, one module
 * per idea:
 * - Civil.mli: the Gregorian calendar computed, not looked up -- a date
 *   is a day number and back, so a month is a range of numbers and a
 *   week is 7 of them from a Sunday;
 * - Julian.mli: the calendar before the switch, cal's famous month:
 *   View > England's calendar and date=1752-09-02 show September 1752
 *   with its eleven missing days, the weekdays going on regardless
 *   (Rome's switch, in October 1582, is the other choice);
 * - Recur.mli: a repeating event is a rule, its occurrences computed
 *   for the days on screen and never stored -- the status line shows
 *   the selected event's rule as iCalendar writes it
 *   (FREQ=WEEKLY;BYDAY=MO,TH);
 * - Ics.mli: the file every calendar exchanges, Export writing it and
 *   Import reading any (from the store: natively the files of
 *   ~/.elm-playground/documents, where another program's export can be
 *   put).
 *
 * Dragging an occurrence of a repeating event moves the whole series
 * (its start, and its weekdays for a weekly rule): iCal asks "this one
 * or all of them?", and "this one" is an exception to the rule
 * (iCalendar's EXDATE and RECURRENCE-ID), which Recur does not have.
 *
 * Uses: Civil, Julian, Recur (core's time/), Ics (appkits/pim), Clock and
 * Playground_platform.utc_offset (today), the gui toolkit (menus,
 * buttons, the title field) and File_menu (saved as an Ics.calendar);
 * not Layout for the calendar itself, whose cells are arithmetic, as
 * TinyExcel's are.
 *
 * Exercises: "this occurrence or all of them?" (EXDATE); overlapping
 * events side by side rather than on top of each other; time zones
 * (an event at 9:00 in New York shown at 15:00 in Paris: the tz
 * database); a year view; the to-do list, Ics's VTODO, beside the
 * month (the Palm Pilot's, plan_pim.md).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type view = Month | Week

(* an event being dragged: where it was grabbed, and whether by its
 * bottom edge *)
type drag = { uid : string; edge : bool; from_day : int; from_sec : int }

type model = {
  cal : Ics.calendar;
  view : view;
  switch : Julian.switch;
  day : int; (* the selected day, a day number (Civil's) *)
  slot : int option; (* the selected hour in the week, seconds after midnight *)
  selected : string option; (* an event's uid *)
  title : string; (* the next event's, being typed *)
  next_uid : int;
  drag : drag option;
  file : File_menu.t;
  was_down : bool;
  enter_was : bool;
  started : bool; (* today and the flags read, at the first frame *)
}

let initial : model =
  { cal = { events = []; todos = [] }; view = Month; switch = Julian.england; day = 0; slot = None;
    selected = None; title = ""; next_uid = 1; drag = None; file = File_menu.start; was_down = false;
    enter_was = false; started = false }

(* the calendar before and after the switch: the second is always
 * Gregorian, cal's choice is England's *)
let gregorian_only : Julian.switch = min_int

(*****************************************************************************)
(* Days and events *)
(*****************************************************************************)

let days (d : Civil.date) : int = Civil.days_from_civil d
let date (n : int) : Civil.date = Civil.civil_from_days n

let fdiv (a : int) (b : int) : int = if a >= 0 then a / b else -(((-a) + b - 1) / b)

let today (computer : computer) : int =
  let t = match computer.time with Time t -> t in
  fst (Clock.split ~offset:(Playground_platform.utc_offset computer.time) t)

(* the days [e] happens on between [from] and [upto] *)
let occurring (e : Ics.event) ~(from : int) ~(upto : int) : int list =
  Ics.occurrences e ~from:(date from) ~upto:(date upto) |> List.map days

(* seconds from its start to its end, an hour if it doesn't say *)
let duration (e : Ics.event) : int =
  match (e.start.time, e.end_) with
  | Some s, Some { date; time = Some t; _ } -> max 1800 (((days date - days e.start.date) * 86400) + t - s)
  | _ -> 3600

let shift (m : Ics.moment) ~(days_by : int) ~(secs : int) : Ics.moment =
  match m.time with
  | None -> { m with date = date (days m.date + days_by) }
  | Some t ->
      let total = ((days m.date + days_by) * 86400) + t + secs in
      let d = fdiv total 86400 in
      { m with date = date d; time = Some (total - (d * 86400)) }

(* the event moved by some days and seconds, its end with it; a weekly
 * rule's weekdays turn with it *)
let move_event (e : Ics.event) ~(days_by : int) ~(secs : int) : Ics.event =
  let rrule =
    Option.map (fun (r : Recur.rule) -> { r with by_day = List.map (fun wd -> (((wd + days_by) mod 7) + 7) mod 7) r.by_day }) e.rrule
  in
  { e with start = shift e.start ~days_by ~secs; end_ = Option.map (shift ~days_by ~secs) e.end_; rrule }

let hhmm (secs : int) : string = Printf.sprintf "%d:%02d" (secs / 3600) (secs / 60 mod 60)

let rule_name (r : Recur.rule option) : string =
  match r with
  | None -> "once"
  | Some r ->
      let every = match r.freq with Daily -> "day" | Weekly -> "week" | Monthly -> "month" | Yearly -> "year" in
      Printf.sprintf "every %s%s (%s)" (if r.interval > 1 then string_of_int r.interval ^ " " else "") every (Ics.rule_to_string r)

(* a few events around today, to have something to look at *)
let sample (today : int) : Ics.event list =
  let sunday = today - Civil.weekday today in
  let at n h m : Ics.moment = { date = date n; time = Some ((h * 3600) + (m * 60)); utc = false } in
  let all_day (d : Civil.date) : Ics.moment = { date = d; time = None; utc = false } in
  let first = date today in
  [ { uid = "standup@tiny"; summary = "Standup"; description = ""; location = "";
      start = at (sunday + 1) 9 0; end_ = Some (at (sunday + 1) 9 30); rrule = Ics.rule_of_string "FREQ=WEEKLY;BYDAY=MO,TH" };
    { uid = "lunch@tiny"; summary = "Lunch with Ada"; description = ""; location = "";
      start = at (today + 2) 12 30; end_ = Some (at (today + 2) 13 30); rrule = None };
    { uid = "swim@tiny"; summary = "Swimming"; description = ""; location = "";
      start = at (sunday + 6) 10 0; end_ = Some (at (sunday + 6) 11 0); rrule = Ics.rule_of_string "FREQ=WEEKLY;INTERVAL=2" };
    { uid = "rent@tiny"; summary = "Pay the rent"; description = ""; location = "";
      start = all_day { first with day = 1 }; end_ = None; rrule = Ics.rule_of_string "FREQ=MONTHLY" };
    (* Ada Lovelace, born 10 December 1815 *)
    { uid = "ada@tiny"; summary = "Ada's birthday"; description = ""; location = "";
      start = all_day { year = 1815; month = 12; day = 10 }; end_ = None; rrule = Ics.rule_of_string "FREQ=YEARLY" } ]

(*****************************************************************************)
(* Geometry: where days, hours and events are *)
(*****************************************************************************)

let header = 28.
let first_hour = 7
let last_hour = 21
let strip = 34. (* the week's all-day events *)
let gutter = 56. (* the week's hours *)

(* the month shown: its first day, and its days (the switch may have
 * taken some) *)
let month_of (m : model) : int * int list =
  let d = Julian.of_days m.switch m.day in
  (Option.get (Julian.to_days m.switch { d with day = 1 }), Julian.month m.switch d.year d.month)

let week_of (m : model) : int = m.day - Civil.weekday m.day

let month_cell (b : Widget.box) (first : int) (n : int) : Widget.box =
  let row = (n - first + Civil.weekday first) / 7 and col = Civil.weekday n in
  let w = b.w /. 7. and h = (b.h -. header) /. 6. in
  { x = Widget.left b +. (w *. (float_of_int col +. 0.5)); y = Widget.top b -. header -. (h *. (float_of_int row +. 0.5)); w; h }

let hours_top (b : Widget.box) = Widget.top b -. header -. strip
let hour_h (b : Widget.box) = (b.h -. header -. strip) /. float_of_int (last_hour - first_hour)
let col_w (b : Widget.box) = (b.w -. gutter) /. 7.
let y_of_secs (b : Widget.box) (s : int) = hours_top b -. ((float_of_int s /. 3600.) -. float_of_int first_hour) *. hour_h b
let col_x (b : Widget.box) (i : int) = Widget.left b +. gutter +. (col_w b *. (float_of_int i +. 0.5))

(* the day (and in the week, the half hour) under the point *)
let day_at (m : model) (b : Widget.box) (px, py) : (int * int option) option =
  if not (Widget.contains b px py) then None
  else
    match m.view with
    | Month ->
        let first, ds = month_of m in
        List.find_opt (fun n -> Widget.contains (month_cell b first n) px py) ds |> Option.map (fun n -> (n, None))
    | Week ->
        let col = int_of_float (Float.floor ((px -. Widget.left b -. gutter) /. col_w b)) in
        if col < 0 || col > 6 || py > Widget.top b -. header then None
        else if py > hours_top b then Some (week_of m + col, None)
        else
          let s = (first_hour * 3600) + int_of_float ((hours_top b -. py) /. hour_h b *. 3600.) in
          Some (week_of m + col, Some (s / 1800 * 1800))

(* every event on screen, with the day it is on and its box: what the
 * view draws and the mouse hits *)
let items (m : model) (b : Widget.box) : (Ics.event * int * Widget.box) list =
  match m.view with
  | Month ->
      let first, ds = month_of m in
      let last = List.fold_left max first ds in
      List.concat_map
        (fun (e : Ics.event) -> List.map (fun n -> (e, n)) (occurring e ~from:first ~upto:last))
        m.cal.events
      |> List.stable_sort (fun ((a : Ics.event), n) ((b : Ics.event), n') ->
             compare (n, Option.value ~default:(-1) a.start.time) (n', Option.value ~default:(-1) b.start.time))
      |> List.fold_left
           (fun (acc, count) ((e : Ics.event), n) ->
             let i = Option.value ~default:0 (List.assoc_opt n count) in
             let cell = month_cell b first n in
             let line : Widget.box = { cell with y = Widget.top cell -. 30. -. (18. *. float_of_int i); h = 16.; w = cell.w -. 6. } in
             ((if i < 3 && List.mem n ds then (e, n, line) :: acc else acc), (n, i + 1) :: List.remove_assoc n count))
           ([], [])
      |> fst |> List.rev
  | Week ->
      let first = week_of m in
      List.concat_map
        (fun (e : Ics.event) ->
          List.mapi
            (fun _ n ->
              let x = col_x b (n - first) and w = col_w b -. 6. in
              match e.start.time with
              | None -> (e, n, ({ x; y = Widget.top b -. header -. (strip /. 2.); w; h = strip -. 8. } : Widget.box))
              | Some s ->
                  let top = y_of_secs b s and bottom = y_of_secs b (min (last_hour * 3600) (s + duration e)) in
                  (e, n, { x; y = (top +. bottom) /. 2.; w; h = max 12. (top -. bottom) }))
            (occurring e ~from:first ~upto:(first + 6)))
        m.cal.events
      |> List.filter (fun ((e : Ics.event), _, _) ->
             match e.start.time with Some s -> s < last_hour * 3600 && s + duration e > first_hour * 3600 | None -> true)

let heading (m : model) : string =
  match m.view with
  | Month ->
      let d = Julian.of_days m.switch m.day in
      Printf.sprintf "%s %d%s" (Civil.month_name d.month) d.year (if m.day < m.switch then " (Julian)" else "")
  | Week ->
      let d = Julian.of_days m.switch (week_of m) in
      Printf.sprintf "week of Sunday, %s %d, %d" (Civil.month_name d.month) d.day d.year

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let kind = { File_menu.magic = "TinyCalendar 1"; extension = ".calendar" }

(* the File menu without its Export, which writes the saved bytes:
 * a calendar is exported as iCalendar instead *)
let file_items = [ "File"; "New"; "Open..."; "Save"; "Save As..." ]
let view_menu = [ "View"; "Month"; "Week"; "England's calendar (1752)"; "Rome's calendar (1582)"; "Gregorian only" ]
let repeat_menu = [ "Repeat"; "Never"; "Daily"; "Weekly"; "Monthly"; "Yearly" ]

type slot = Menu_file | Menu_view | Menu_repeat | Menu_import | Export | Prev | Today | Next | Heading | Grid | Title | Add | Delete | Status

let panel =
  Layout.(
    center
      (column ~gap:10.
         [ row ~gap:8.
             [ leaf Menu_file (Gui.menu_size file_items); leaf Menu_view (Gui.menu_size view_menu);
               leaf Menu_repeat (Gui.menu_size repeat_menu); leaf Menu_import (Gui.menu_size [ "Import a long name.ics" ]);
               leaf Export (Gui.button_size "Export .ics") ];
           row ~gap:8.
             [ leaf Prev (Gui.button_size "<"); leaf Today (Gui.button_size "Today"); leaf Next (Gui.button_size ">");
               leaf Heading (Gui.label_size "week of Sunday, September 27, 2026 (Julian)") ];
           leaf Grid (940., 640.);
           row ~gap:8. [ stretch (leaf Title (Gui.field_size ())); leaf Add (Gui.button_size "Add"); leaf Delete (Gui.button_size "Delete") ];
           leaf Status (Gui.label_size (String.make 70 'x')) ]))

let reopened (r : Ics.calendar File_menu.result) (m : model) : model =
  match r with
  | File_menu.Nothing -> m
  | File_menu.New -> { m with cal = { events = []; todos = [] }; selected = None }
  | File_menu.Opened cal -> { m with cal; selected = None }

let from_flags (computer : computer) (m : model) : model =
  let flags = computer.flags in
  let switch =
    match List.assoc_opt "switch" flags with Some "rome" -> Julian.rome | Some "none" -> gregorian_only | _ -> Julian.england
  in
  let t = today computer in
  let day =
    match Option.map (String.split_on_char '-') (List.assoc_opt "date" flags) with
    | Some [ y; mo; d ] -> (
        match (int_of_string_opt y, int_of_string_opt mo, int_of_string_opt d) with
        | Some year, Some month, Some day -> Option.value ~default:t (Julian.to_days switch { year; month; day })
        | _ -> t)
    | _ -> t
  in
  { m with switch; day; view = (if List.assoc_opt "view" flags = Some "week" then Week else Month);
    cal = { events = sample t; todos = [] }; started = true }

let add (m : model) : model =
  if String.trim m.title = "" then m
  else
    let start : Ics.moment = { date = date m.day; time = (if m.view = Week then m.slot else None); utc = false } in
    let e : Ics.event =
      { uid = Printf.sprintf "%d@tiny" m.next_uid; summary = String.trim m.title; description = ""; location = "";
        start; end_ = None; rrule = None }
    in
    { m with cal = { m.cal with events = m.cal.events @ [ e ] }; title = ""; selected = Some e.uid; next_uid = m.next_uid + 1 }

(* an imported event at a UTC time (20260928T070000Z) shown at ours:
 * shifted by the offset in force at that instant (summer's or
 * winter's, the platform knows), then floating like the others *)
let local (e : Ics.event) : Ics.event =
  let here (m : Ics.moment) : Ics.moment =
    match m.time with
    | Some t when m.utc ->
        let instant = float_of_int ((days m.date * 86400) + t) in
        { (shift m ~days_by:0 ~secs:(Playground_platform.utc_offset (Time instant) * 60)) with utc = false }
    | _ -> m
  in
  { e with start = here e.start; end_ = Option.map here e.end_ }

let edit (m : model) (f : Ics.event -> Ics.event) : model =
  match m.selected with
  | None -> m
  | Some uid -> { m with cal = { m.cal with events = List.map (fun (e : Ics.event) -> if e.uid = uid then f e else e) m.cal.events } }

(* the drag let go at a day (and a half hour) *)
let drop (m : model) (d : drag) (target : (int * int option) option) : model =
  match target with
  | None -> m
  | Some (n, s) ->
      edit { m with selected = Some d.uid; day = n } (fun e ->
          match (d.edge, e.start.time, s) with
          | true, Some start, Some s ->
              let until = max (start + 1800) (s + 1800) in
              { e with end_ = Some { e.start with time = Some until } }
          | _ ->
              let secs = match (e.start.time, s) with Some _, Some s -> s - d.from_sec | _ -> 0 in
              move_event e ~days_by:(n - d.from_day) ~secs)

let mouse (computer : computer) (b : Widget.box) (m : model) : model =
  let mo = computer.mouse in
  let p = (mo.mx, mo.my) in
  if mo.mdown && not m.was_down then
    match List.find_opt (fun (_, _, box) -> Widget.contains box mo.mx mo.my) (List.rev (items m b)) with
    | Some (e, n, box) ->
        let from_sec = match day_at m b p with Some (_, Some s) -> s | _ -> 0 in
        let edge = m.view = Week && e.start.time <> None && mo.my < Widget.bottom box +. 6. in
        { m with selected = Some e.uid; day = n; drag = Some { uid = e.uid; edge; from_day = n; from_sec } }
    | None -> (
        match day_at m b p with Some (n, s) -> { m with day = n; slot = s; selected = None } | None -> m)
  else if (not mo.mdown) && m.was_down then
    match m.drag with Some d -> { (drop m d (day_at m b p)) with drag = None } | None -> m
  else m

let update (caps : File_menu.caps) (computer : computer) (m : model) : model =
  let m = if m.started then m else from_flags computer m in
  let current () = m.cal in
  if File_menu.busy m.file then
    let file, r = File_menu.dialog caps kind computer ~current m.file in
    reopened r { m with file; was_down = computer.mouse.mdown }
  else
    let at = Layout.arrange (Gui.area computer) panel in
    let box slot : Widget.box = List.assoc slot at in
    let m = if Gui.modal () then m else mouse computer (box Grid) m in
    let m =
      let file, r = File_menu.menu_in ~items:file_items caps kind computer (box Menu_file) ~current m.file in
      reopened r { m with file }
    in
    let m =
      match List.nth_opt view_menu (Gui.menu_in computer (box Menu_view) view_menu 0) with
      | Some "Month" -> { m with view = Month }
      | Some "Week" -> { m with view = Week }
      | Some "England's calendar (1752)" -> { m with switch = Julian.england }
      | Some "Rome's calendar (1582)" -> { m with switch = Julian.rome }
      | Some "Gregorian only" -> { m with switch = gregorian_only }
      | _ -> m
    in
    let m =
      let rule s = fun (e : Ics.event) -> { e with rrule = Ics.rule_of_string s } in
      match List.nth_opt repeat_menu (Gui.menu_in computer (box Menu_repeat) repeat_menu 0) with
      | Some "Never" -> edit m (fun e -> { e with rrule = None })
      | Some "Daily" -> edit m (rule "FREQ=DAILY")
      | Some "Weekly" -> edit m (rule "FREQ=WEEKLY")
      | Some "Monthly" -> edit m (rule "FREQ=MONTHLY")
      | Some "Yearly" -> edit m (rule "FREQ=YEARLY")
      | _ -> m
    in
    (* Import: the iCalendar files of the store, read by Ics, added *)
    let ics = List.filter (fun n -> Filename.check_suffix n ".ics") (Playground_platform.stored caps) in
    let m =
      match List.nth_opt ("Import" :: ics) (Gui.menu_in computer (box Menu_import) ("Import" :: ics) 0) with
      | Some name when name <> "Import" -> (
          match Playground_platform.fetch caps name with
          | Some text -> { m with cal = { m.cal with events = m.cal.events @ List.map local (Ics.of_string text).events } }
          | None -> m)
      | _ -> m
    in
    if Gui.button_in computer (box Export) "Export .ics" then begin
      let t = match computer.time with Time t -> t in
      let d, tod = Clock.local ~offset:0 t in
      let stamp : Ics.moment = { date = d; time = Some ((tod.hour * 3600) + (tod.minute * 60) + int_of_float tod.second); utc = true } in
      Playground_platform.export caps (Filename.remove_extension (File_menu.title m.file) ^ ".ics") (Ics.to_string ~stamp m.cal)
    end;
    let m =
      (* a month as people wrote it: its 1st, in the calendar of then *)
      let jump by =
        match m.view with
        | Week -> { m with day = m.day + (7 * by) }
        | Month ->
            let d = Julian.of_days m.switch m.day in
            let d = Civil.add_months { d with day = 1 } by in
            { m with day = Option.value ~default:m.day (Julian.to_days m.switch d) }
      in
      if Gui.button_in computer (box Prev) "<" then jump (-1)
      else if Gui.button_in computer (box Next) ">" then jump 1
      else if Gui.button_in computer (box Today) "Today" then { m with day = today computer }
      else m
    in
    Gui.label_in computer (box Heading) (heading m);
    let title = Gui.field_in computer (box Title) m.title in
    let enter = computer.keyboard.kenter && not m.enter_was in
    let m = { m with title } in
    let m = if Gui.button_in computer (box Add) "Add" || enter then add m else m in
    let m =
      if Gui.button_in computer (box Delete) "Delete" then
        { m with cal = { m.cal with events = List.filter (fun (e : Ics.event) -> Some e.uid <> m.selected) m.cal.events }; selected = None }
      else m
    in
    let status =
      match (File_menu.said m.file, List.find_opt (fun (e : Ics.event) -> Some e.uid = m.selected) m.cal.events) with
      | said, _ when said <> "" -> said
      | _, Some e ->
          Printf.sprintf "%s: %s%s" e.summary
            (match e.start.time with Some s -> hhmm s ^ "-" ^ hhmm (s + duration e) ^ ", " | None -> "all day, ")
            (rule_name e.rrule)
      | _ -> File_menu.title m.file
    in
    Gui.label_in computer (box Status) status;
    { m with was_down = computer.mouse.mdown; enter_was = computer.keyboard.kenter }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale (size /. words_font_size)

(* [s] from the left of [b], cut to fit *)
let left_text (color : color) (size : number) (b : Widget.box) (s : string) : shape =
  let fits = max 1 (int_of_float (b.w /. (0.6 *. size))) in
  let s = if String.length s > fits then String.sub s 0 (max 1 (fits - 1)) ^ "." else s in
  text color size s |> move (Widget.left b +. 4. +. (Widget.text_width ~size s /. 2.)) b.y

let line (color : color) (x1, y1) (x2, y2) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (max 1. (Float.abs dx)) (max 1. (Float.abs dy)) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let event_color (e : Ics.event) = if e.rrule = None then rgb 90 140 220 else rgb 110 180 120

let view_month (th : Theme.t) (m : model) (b : Widget.box) (today : int) : shape list =
  let first, ds = month_of m in
  let w = b.w /. 7. in
  let names = List.init 7 (fun i -> text th.text 15. (String.sub (Civil.weekday_name i) 0 2) |> move (Widget.left b +. (w *. (float_of_int i +. 0.5))) (Widget.top b -. (header /. 2.))) in
  let cells =
    List.concat_map
      (fun n ->
        let c = month_cell b first n in
        let d = Julian.of_days m.switch n in
        let number : Widget.box = { c with y = Widget.top c -. 12.; h = 18. } in
        [ rectangle (if n = m.day then rgb 255 245 200 else th.face) (c.w -. 2.) (c.h -. 2.) |> move c.x c.y;
          left_text (if n = today then red else th.text) 15. number (string_of_int d.day) ])
      ds
  in
  names @ cells

let view_week (th : Theme.t) (m : model) (b : Widget.box) (today : int) : shape list =
  let first = week_of m in
  let cw = col_w b in
  let days =
    List.init 7 (fun i ->
        let n = first + i in
        let d = Julian.of_days m.switch n in
        let x = col_x b i in
        [ rectangle (if n = m.day then rgb 255 245 200 else th.face) (cw -. 2.) (b.h -. header) |> move x (Widget.top b -. header -. ((b.h -. header) /. 2.));
          text (if n = today then red else th.text) 15. (Printf.sprintf "%s %d" (String.sub (Civil.weekday_name i) 0 3) d.day) |> move x (Widget.top b -. (header /. 2.)) ])
    |> List.concat
  in
  let hours =
    List.init (last_hour - first_hour + 1) (fun i ->
        let h = first_hour + i in
        let y = y_of_secs b (h * 3600) in
        [ line th.edge (Widget.left b +. gutter, y) (Widget.right b, y);
          text th.text 13. (Printf.sprintf "%d:00" h) |> move (Widget.left b +. (gutter /. 2.)) (y -. 8.) ])
    |> List.concat
  in
  let slot =
    match m.slot with
    | Some s when m.day >= first && m.day < first + 7 ->
        let top = y_of_secs b s and bottom = y_of_secs b (s + 1800) in
        [ rectangle (rgb 250 220 120) (cw -. 2.) (top -. bottom) |> move (col_x b (m.day - first)) ((top +. bottom) /. 2.) ]
    | _ -> []
  in
  days @ slot @ hours @ [ line th.edge (Widget.left b +. gutter, hours_top b) (Widget.right b, hours_top b) ]

let view (computer : computer) (m : model) : shape list =
  let th = Gui.theme () in
  let at = Layout.arrange (Gui.area computer) panel in
  let b = List.assoc Grid at in
  let today = today computer in
  let grid = match m.view with Month -> view_month th m b today | Week -> view_week th m b today in
  let events =
    List.concat_map
      (fun ((e : Ics.event), _, (box : Widget.box)) ->
        let label = match (e.start.time, m.view) with Some s, Month -> hhmm s ^ " " ^ e.summary | _ -> e.summary in
        let chosen = Some e.uid = m.selected in
        [ rectangle (if chosen then rgb 230 120 60 else event_color e) box.w box.h |> move box.x box.y;
          left_text white 13. { box with y = (if m.view = Week then Widget.top box -. 9. else box.y) } label ])
      (items m b)
  in
  (* where a dragged event would land *)
  let ghost =
    match (m.drag, day_at m b (computer.mouse.mx, computer.mouse.my)) with
    | Some _, Some _ -> [ circle (rgb 230 120 60) 6. |> move computer.mouse.mx computer.mouse.my ]
    | _ -> []
  in
  (rectangle th.background computer.screen.width computer.screen.height :: grid)
  @ events @ ghost @ File_menu.view m.file @ Gui.draw ()

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> File_menu.caps)))
