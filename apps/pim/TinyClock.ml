(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Macintosh's Alarm Clock (Apple, 1984), the desk
 * accessory showing the time in a strip and ringing once a day, with
 * the face of xclock (the X Window System's, in the mid-1980s): the
 * time you read on a wall, the date, an alarm, and a row of cities.
 *
 *   d          the analog face or the digital strip
 *   a          the alarm on or off
 *   up, down   the alarm a minute later or earlier
 *   left, right  an hour earlier or later
 *   space      stop it ringing
 *
 * flags face=digital, and alarm=7:30 (set and on).
 *
 * What it teaches is Clock.mli: an animation counts seconds from
 * wherever it likes, but a clock counts them from the Unix epoch
 * (1970-01-01, UTC) and must then say what they are *here* -- the
 * offset, which only the platform knows (Playground_platform.utc_offset:
 * the C library's zone natively, the browser's on the web). And the two
 * faces read the same seconds two ways: the digital strip truncates
 * them (14:42:15), while the hands keep every fraction, the hour hand
 * moving a little every minute and the second hand sweeping.
 *
 * The cities are the honest kind: none changes its clocks in summer,
 * so an offset written here is right all year. Paris or New York
 * would need the rules of their daylight saving (the tz database, which
 * the platform has and this program doesn't): that is why the local
 * offset is asked of the platform and not computed.
 *
 * Uses: Civil and Clock (core), Playground_platform.utc_offset,
 * Scene2d (keys pressed, not held), Audio (the alarm's beeps); not the
 * gui toolkit.
 *
 * Exercises: a city typed in with its offset; a chime on the hour
 * (Westminster's quarters, with Audio.abc); the alarm kept across runs
 * (Playground_platform.store); the Mac's other desk accessories, a
 * stopwatch and a timer counting down.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type face = Analog | Digital

type clock = {
  face : face;
  alarm : int; (* minutes after local midnight *)
  alarm_on : bool;
  (* the minute the ringing was stopped in (local minutes since the
   * epoch): the alarm rings through its minute unless stopped *)
  silenced : int option;
  started : bool; (* the flags read *)
}

type model = clock Scene2d.t

let initial_model : model =
  Scene2d.start { face = Analog; alarm = 7 * 60; alarm_on = false; silenced = None; started = false }

(* cities with no daylight saving, so one offset all year (see the
 * header) *)
let cities = [ ("Honolulu", -600); ("Phoenix", -420); ("Reykjavik", 0); ("Mumbai", 330); ("Kathmandu", 345); ("Tokyo", 540) ]

(*****************************************************************************)
(* The time *)
(*****************************************************************************)

let seconds (computer : computer) : float = match computer.time with Time t -> t

(* the local minutes since the epoch, what the alarm compares *)
let local_minute ~(offset : int) (t : float) : int = int_of_float (Float.floor ((t /. 60.) +. float_of_int offset))

let ringing ~(offset : int) (t : float) (c : clock) : bool =
  let m = local_minute ~offset t in
  c.alarm_on && ((m mod 1440) + 1440) mod 1440 = c.alarm && c.silenced <> Some m

(* "2:42 PM", the Mac's way *)
let twelve_hour ?(seconds = true) (hour : int) (minute : int) (second : int) : string =
  let h = if hour mod 12 = 0 then 12 else hour mod 12 in
  let ampm = if hour < 12 then "AM" else "PM" in
  if seconds then Printf.sprintf "%d:%02d:%02d %s" h minute second ampm else Printf.sprintf "%d:%02d %s" h minute ampm

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let from_flags (flags : flags) (c : clock) : clock =
  let c = if List.assoc_opt "face" flags = Some "digital" then { c with face = Digital } else c in
  let c =
    match Option.map (String.split_on_char ':') (List.assoc_opt "alarm" flags) with
    | Some [ h; m ] -> (
        match (int_of_string_opt h, int_of_string_opt m) with
        | Some h, Some m when h >= 0 && h < 24 && m >= 0 && m < 60 -> { c with alarm = (h * 60) + m; alarm_on = true }
        | _ -> c)
    | _ -> c
  in
  { c with started = true }

let move_alarm (c : clock) (minutes : int) : clock = { c with alarm = (((c.alarm + minutes) mod 1440) + 1440) mod 1440 }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed (key : keyboard -> bool) = Scene2d.pressed key s in
  let key name = pressed (fun k -> Set_.mem name k.keys) in
  let c = if s.scene.started then s.scene else from_flags computer.flags s.scene in
  let t = seconds computer in
  let offset = Playground_platform.utc_offset computer.time in
  let c =
    if ringing ~offset t c && (pressed (fun k -> k.kspace) || pressed (fun k -> k.kenter)) then
      { c with silenced = Some (local_minute ~offset t) }
    else if key "d" then { c with face = (if c.face = Analog then Digital else Analog) }
    else if key "a" then { c with alarm_on = not c.alarm_on }
    else if pressed (fun k -> k.kup) then move_alarm c 1
    else if pressed (fun k -> k.kdown) then move_alarm c (-1)
    else if pressed (fun k -> k.kright) then move_alarm c 60
    else if pressed (fun k -> k.kleft) then move_alarm c (-60)
    else c
  in
  (* beeping during the first half of every second, as long as it rings *)
  if ringing ~offset t c && t -. Float.floor t < 0.5 then Audio.keep_playing "alarm" (Audio.square 880.);
  { s with scene = c }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let paper = rgb 250 250 245
let background = rgb 200 200 200
let gray = rgb 100 100 100

(* [at r angle]: the point [r] from the center, [angle] degrees
 * clockwise from 12 -- the clock's way, not the playground's (counter-
 * clockwise from 3) *)
let at (r : number) (angle : number) : number * number =
  let a = angle *. Float.pi /. 180. in
  (r *. sin a, r *. cos a)

(* a hand, xclock's thin diamond, pointing at [angle] *)
let hand (color : color) ~(length : number) ~(width : number) (angle : number) : shape =
  polygon color [ (0., length); (width, 0.); (0., -.(width *. 2.)); (-.width, 0.) ] |> rotate (-.angle)

(* the hands keep the fractions: at 2:30 the hour hand is halfway
 * between the 2 and the 3 *)
let face ~(r : number) ~(second_hand : bool) (tod : Clock.time_of_day) : shape =
  let s = tod.second in
  let m = float_of_int tod.minute +. (s /. 60.) in
  let h = float_of_int (tod.hour mod 12) +. (m /. 60.) in
  let ticks =
    List.init (if second_hand then 60 else 12) (fun i ->
        let angle = float_of_int i *. (if second_hand then 6. else 30.) in
        let big = (not second_hand) || i mod 5 = 0 in
        let len = if big then r *. 0.12 else r *. 0.04 in
        let x, y = at (r -. (r *. 0.05) -. (len /. 2.)) angle in
        rectangle black (if big then r *. 0.035 else r *. 0.015) len |> rotate (-.angle) |> move x y)
  in
  group
    ([ circle black (r +. 3.); circle paper r ]
    @ ticks
    @ [ hand black ~length:(r *. 0.55) ~width:(r *. 0.06) (h *. 30.); hand black ~length:(r *. 0.82) ~width:(r *. 0.045) (m *. 6.) ]
    @ (if second_hand then [ hand red ~length:(r *. 0.88) ~width:(r *. 0.015) (s *. 6.) ] else [])
    @ [ circle black (r *. 0.04) ])

(* the Alarm Clock's strip, black on white *)
let strip (tod : Clock.time_of_day) : shape =
  group
    [ rectangle black 706. 206.; rectangle paper 700. 200.;
      text black 7. (twelve_hour tod.hour tod.minute (int_of_float tod.second)) ]

let view_world (t : float) : shape list =
  List.mapi
    (fun i (name, offset) ->
      let _, tod = Clock.split ~offset t in
      let x = (float_of_int i -. 2.5) *. 160. in
      group
        [ face ~r:52. ~second_hand:false tod |> move_y 30.;
          text black 1.6 name |> move_y (-45.);
          text black 1.4 (Clock.to_string ~seconds:false tod ^ "  " ^ Clock.offset_to_string offset) |> move_y (-70.) ]
      |> move x (-310.))
    cities

let view (computer : computer) (s : model) : shape list =
  let c = s.scene in
  let t = seconds computer in
  let offset = Playground_platform.utc_offset computer.time in
  let date, tod = Clock.local ~offset t in
  let weekday = Civil.weekday (Civil.days_from_civil date) in
  let alarm =
    Printf.sprintf "alarm %s  %s" (twelve_hour ~seconds:false (c.alarm / 60) (c.alarm mod 60) 0) (if c.alarm_on then "on" else "off")
  in
  [ rectangle background computer.screen.width computer.screen.height;
    text black 3. "TINY CLOCK" |> move_y 440. ]
  @ [ (match c.face with Analog -> face ~r:230. ~second_hand:true tod | Digital -> strip tod) |> move_y 150. ]
  @ [ text black 2.4
        (Printf.sprintf "%s, %s %d, %d" (Civil.weekday_name weekday) (Civil.month_name date.month) date.day date.year)
      |> move_y (-125.);
      text black 1.6 (Printf.sprintf "here: UTC%s" (Clock.offset_to_string offset)) |> move_y (-160.);
      (if ringing ~offset t c then
         (* flashing, as the Mac's menu bar icon did *)
         if t -. Float.floor t < 0.5 then text red 2.4 "ALARM!  space to stop it" else group []
       else text (if c.alarm_on then black else gray) 2. alarm)
      |> move_y (-195.) ]
  @ view_world t
  @ [ text gray 1.5 "d: analog/digital   a: alarm on/off   up/down: alarm minutes   left/right: its hours   space: stop it"
      |> move_y (-425.) ]

let app = game view update initial_model

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
