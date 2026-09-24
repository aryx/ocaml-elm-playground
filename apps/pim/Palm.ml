(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Palm.mli *)
open Playground

(*****************************************************************************)
(* The data *)
(*****************************************************************************)

type data = {
  events : Ics.event list;
  cards : Vcard.card list;
  todos : Ics.todo list;
  memos : string list;
  next_id : int;
}

let uid (d : data) : string * data = (Printf.sprintf "%d@palm" d.next_id, { d with next_id = d.next_id + 1 })

let sample (today : int) : data =
  let date n = Civil.civil_from_days n in
  let at n h m : Ics.moment = { date = date n; time = Some ((h * 3600) + (m * 60)); utc = false } in
  let event uid summary start end_ rrule : Ics.event =
    { uid; summary; description = ""; location = ""; start; end_; rrule = Option.bind rrule Ics.rule_of_string }
  in
  let monday = today - Civil.weekday today + 1 in
  (* fictional numbers: 555-01xx is reserved for them *)
  let card family given org phone email : Vcard.card =
    { (Vcard.make (given ^ " " ^ family)) with
      uid = String.lowercase_ascii family ^ "@palm";
      name = { family; given; additional = ""; prefix = ""; suffix = "" };
      org;
      phones = [ { number = phone; kinds = [ "work" ] } ];
      emails = [ { address = email; kinds = [ "internet" ] } ] }
  in
  let todo uid summary priority due completed : Ics.todo =
    { uid; summary; priority; completed; due = Option.map (fun n : Ics.moment -> { date = date n; time = None; utc = false }) due }
  in
  { events =
      [ event "standup@palm" "Standup" (at monday 9 0) (Some (at monday 9 30)) (Some "FREQ=WEEKLY;BYDAY=MO,TH");
        event "lunch@palm" "Lunch with Ada" (at (today + 2) 12 30) (Some (at (today + 2) 13 30)) None;
        event "dentist@palm" "Dentist" (at today 16 0) (Some (at today 17 0)) None;
        event "rent@palm" "Pay the rent" { date = { (date today) with day = 1 }; time = None; utc = false } None
          (Some "FREQ=MONTHLY") ];
    cards =
      [ card "Lovelace" "Ada" "Analytical Engine Society" "555-0101" "ada@example.org";
        card "Babbage" "Charles" "Analytical Engine Society" "555-0102" "charles@example.org";
        card "Hopper" "Grace" "US Navy" "555-0103" "grace@example.org";
        card "Turing" "Alan" "University of Manchester" "555-0104" "alan@example.org";
        card "Hawkins" "Jeff" "Palm Computing" "555-0105" "jeff@example.org";
        card "Dubinsky" "Donna" "Palm Computing" "555-0106" "donna@example.org" ];
    todos =
      [ todo "plan@palm" "Write the plan" 1 (Some today) false;
        todo "rfc@palm" "Read RFC 5545" 2 (Some (today + 3)) false;
        (* the Pilot ran on two AAA batteries, for weeks *)
        todo "aaa@palm" "Buy AAA batteries" 3 None false;
        todo "sync@palm" "HotSync with the desktop" 2 (Some (today - 1)) true;
        todo "graffiti@palm" "Learn Graffiti" 4 None false ];
    memos =
      [ "Groceries\nmilk\neggs\ncoffee";
        "Graffiti\none stroke per letter, each drawn\nthe way the letter looks";
        "The four buttons\nDate Book, Address, To Do, Memo Pad:\nwhat the Pilot did, and all it did" ];
    next_id = 1 }

(*****************************************************************************)
(* The stylus and the keys *)
(*****************************************************************************)

type input = {
  tap : (float * float) option;
  typed : string;
  enter : bool;
  backspace : bool;
  tab : bool;
  up : bool;
  down : bool;
  today : int;
}

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

let size = 160.
let dots = 4.
let center = (0., 105.)

let to_screen (x, y) : float * float =
  let cx, cy = center in
  (cx -. (size *. dots /. 2.) +. (x *. dots), cy +. (size *. dots /. 2.) -. (y *. dots))

let of_screen (sx, sy) : (float * float) option =
  let cx, cy = center in
  let x = (sx -. cx +. (size *. dots /. 2.)) /. dots and y = (cy +. (size *. dots /. 2.) -. sy) /. dots in
  if x >= 0. && x < size && y >= 0. && y < size then Some (x, y) else None

(* the Pilot's LCD, greenish grey, and the greys of the Palm III's
 * 2-bit screen *)
let paper = rgb 186 196 160
let light = rgb 150 160 128
let mid = rgb 96 104 80
let ink = rgb 28 34 24

type box = float * float * float * float

let inside ((x, y, w, h) : box) (px, py) : bool = px >= x && px < x +. w && py >= y && py < y +. h

let rect (color : color) ((x, y, w, h) : box) : shape =
  let sx, sy = to_screen (x +. (w /. 2.), y +. (h /. 2.)) in
  rectangle color (w *. dots) (h *. dots) |> move sx sy

(* The font. The Palm drew its own, dot by dot; this one draws Hershey's
 * strokes itself (graphics/font) rather than asking the playground's
 * [words], whose font differs from a backend to another: the same
 * glyphs give the widths and the strokes, so a line is exactly as wide
 * as measured, and a column of phone numbers lines up on its right.
 * An em of 9 dots; Hershey's baseline is at y = 9, y down. *)
let em = 9.
let k = em /. Hershey.units_per_em (* dots per font unit *)
let width (s : string) : float = snd (Hershey.layout s) *. k

(* a stroke, from one Palm point to the next, as a thin rectangle one
 * pen-width longer, so that the joints overlap *)
let segment (color : color) (pen : float) (p1 : float * float) (p2 : float * float) : shape =
  let x1, y1 = to_screen p1 and x2, y2 = to_screen p2 in
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (sqrt ((dx *. dx) +. (dy *. dy)) +. pen) pen
  |> rotate (atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let fit (w : float) (s : string) : string =
  if width s <= w then s
  else
    let rec cut n = if n <= 0 || width (String.sub s 0 n ^ ".") <= w then String.sub s 0 (max 0 n) ^ "." else cut (n - 1) in
    cut (String.length s)

let text ?(color = ink) ?(bold = false) ~(x : float) ~(y : float) (s : string) : shape =
  let strokes, _ = Hershey.layout s in
  (* bold: a thicker pen, as the plotters were bold *)
  let pen = (if bold then 1.3 else 0.8) *. dots in
  let baseline = y +. 8. in
  let at (gx, gy) = (x +. (gx *. k), baseline +. ((gy -. 9.) *. k)) in
  let rec segments = function
    | a :: (b :: _ as rest) -> segment color pen (at a) (at b) :: segments rest
    | _ -> []
  in
  group (List.concat_map segments strokes)

let text_right ?(color = ink) ~(x : float) ~(y : float) (s : string) : shape = text ~color ~x:(x -. width s) ~y s

let title (s : string) : shape list =
  let w = width s +. 8. in
  [ rect ink (0., 0., w, 12.); rect ink (0., 11., size, 1.); text ~color:paper ~bold:true ~x:4. ~y:0.5 s ]

let buttons (labels : string list) : (string * box) list =
  List.fold_left
    (fun (acc, x) l ->
      let w = width l +. 10. in
      ((l, (x, 147., w, 12.)) :: acc, x +. w +. 4.))
    ([], 1.) labels
  |> fst |> List.rev

(* the Palm's buttons: a rounded frame, drawn as a frame with its
 * corners cut *)
let draw_buttons (bs : (string * box) list) : shape list =
  List.concat_map
    (fun (l, ((x, y, w, h) as b)) ->
      [ rect ink b; rect paper (x +. 1., y +. 1., w -. 2., h -. 2.); rect paper (x, y, 1., 1.); rect paper (x +. w -. 1., y, 1., 1.);
        rect paper (x, y +. h -. 1., 1., 1.); rect paper (x +. w -. 1., y +. h -. 1., 1., 1.); text ~x:(x +. 5.) ~y:(y +. 1.) l ])
    bs

let tapped (i : input) (bs : (string * box) list) : string option =
  match i.tap with Some p -> List.find_map (fun (l, b) -> if inside b p then Some l else None) bs | None -> None

let checkbox ~(x : float) ~(y : float) (ticked : bool) : shape list =
  [ rect ink (x, y +. 2., 7., 7.); rect paper (x +. 1., y +. 3., 5., 5.) ]
  @ if ticked then [ rect ink (x +. 2., y +. 4., 3., 3.) ] else []

let row_at ~(y0 : float) ~(rows : int) (i : input) : int option =
  match i.tap with
  | Some (_, y) when y >= y0 ->
      let r = int_of_float ((y -. y0) /. 11.) in
      if r < rows then Some r else None
  | _ -> None

let caret ~(time : float) ~(x : float) ~(y : float) (s : string) : shape list =
  if Float.rem time 1. < 0.5 then [ rect ink (x +. width s +. 1., y +. 1., 1., 9.) ] else []
