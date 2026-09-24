(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Pilot (Jeff Hawkins, Donna Dubinsky and Ed
 * Colligan, Palm Computing, 1996), the organizer that fit in a shirt
 * pocket because it did four things: a Date Book, an Address book, a
 * To Do list and a Memo Pad, one hard button each under a 160 x 160
 * screen.
 *
 *   the four round buttons (or F1..F4)   Date Book, Address, To Do, Memo
 *   the rocker (or the up/down arrows)   the day before/after, the list
 *                                        scrolled
 *   the stylus (the mouse)               tap a line, a box, a button
 *   the keyboard                         what Graffiti wrote (see below)
 *
 * flags app=address, app=todo, app=memo: the application shown first.
 *
 * What it teaches:
 * - doing four things and nothing else: the Newton (Apple, 1993) had
 *   tried to do everything, recognizing handwriting as you wrote it,
 *   and failed; Hawkins carried a block of wood the Pilot's size in his
 *   pocket for weeks, asking of every feature whether it fit. Each
 *   application here is a module a page or two long (Pim_date_book,
 *   Pim_address, Pim_todo, Pim_memo), over the screen of Palm.mli;
 * - no Save, no waiting: the Pilot was on the instant its button was
 *   pressed, where it was left, and kept every change the moment it was
 *   made -- here, the four databases stored (Saved, in the store:
 *   Playground_platform.store) at every frame that changed them, and
 *   read back at the start;
 * - written straight on the screen: an event is written on its hour's
 *   line, a name found by writing its first letters in the Look Up line
 *   (Pim_address.mli) -- no dialog asking for what the screen already
 *   shows;
 * - the data is the world's: events and to-dos are iCalendar's (Ics),
 *   addresses are vCards (Vcard), the formats of appkits/pim that
 *   every other calendar and address book reads -- which is what
 *   HotSync, the cradle's one button, relied on to copy them to the
 *   desktop and back.
 *
 * Graffiti, the Pilot's alphabet -- each letter one stroke, drawn the
 * way the letter looks (an A without its bar, a T in one stroke), so
 * the machine never had to guess what a person meant, the person
 * learned what the machine read -- is left for later: here the keyboard
 * stands in for it.
 *
 * Uses: Palm (the screen, the stylus, the data), Ics and Vcard
 * (appkits/pim), Civil, Clock and Recur (core's time/), Saved
 * (appkits/document), Playground_platform.utc_offset (today) and
 * store/fetch; not the gui toolkit (the Palm's widgets are its own, a
 * few dots each) nor File_menu (there is no File menu).
 *
 * Exercises: Graffiti, the strokes recognized from the mouse's path
 * (ai/'s classifiers, or the $1 recognizer, Wobbrock 2007); HotSync,
 * two copies edited apart and merged record by record, a conflict
 * shown as the Palm did, both kept (over networking's Sim_net); the
 * Date Book's week view; categories (Business, Personal), the Palm's
 * one way of sorting anything; Find, across the four databases.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type app = Date_book | Address | Todo | Memo

type model = {
  data : Palm.data;
  saved : Palm.data; (* what the store holds *)
  app : app;
  date_book : Pim_date_book.t;
  address : Pim_address.t;
  todo : Pim_todo.t;
  memo : Pim_memo.t;
  before : keyboard;
  was_down : bool;
  started : bool; (* the store read, at the first frame *)
}

let empty : Palm.data = { events = []; cards = []; todos = []; memos = []; next_id = 1 }

let initial : model =
  { data = empty; saved = empty; app = Date_book; date_book = Pim_date_book.start 0; address = Pim_address.start;
    todo = Pim_todo.start; memo = Pim_memo.start; before = initial_computer.keyboard; was_down = false; started = false }

let name = "TinyPalmPilot.palm"
let magic = "TinyPalmPilot 1"

let today (computer : computer) : int =
  let t = match computer.time with Time t -> t in
  fst (Clock.split ~offset:(Playground_platform.utc_offset computer.time) t)

(*****************************************************************************)
(* The case: where the buttons are *)
(*****************************************************************************)

let hard_buttons = [ (Date_book, -290., "Date Book"); (Address, -160., "Address"); (Todo, 160., "To Do"); (Memo, 290., "Memo Pad") ]
let buttons_y = -392.
let rocker_up = (0., -372.) and rocker_down = (0., -414.)

let near (x, y) (bx, by) (r : number) = ((x -. bx) ** 2.) +. ((y -. by) ** 2.) < r *. r

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let start (caps : < Cap.open_in ; .. >) (computer : computer) (m : model) : model =
  let today = today computer in
  let data =
    match Option.bind (Playground_platform.fetch caps name) (Saved.of_string ~magic) with
    | Some d -> d
    | None -> Palm.sample today
  in
  let app =
    match List.assoc_opt "app" computer.flags with
    | Some "address" -> Address
    | Some "todo" -> Todo
    | Some "memo" -> Memo
    | _ -> Date_book
  in
  { m with data; saved = data; app; date_book = Pim_date_book.start today; started = true }

let update (caps : < Cap.open_in ; Cap.open_out >) (computer : computer) (m : model) : model =
  let m = if m.started then m else start caps computer m in
  let k = computer.keyboard and mo = computer.mouse in
  let pressed name = Set_.mem name k.keys && not (Set_.mem name m.before.keys) in
  let press = mo.mdown && not m.was_down in
  let p = (mo.mx, mo.my) in
  let clicked (at : number * number) (r : number) = press && near p at r in
  (* the hard buttons, pressed or clicked *)
  let app =
    List.fold_left
      (fun app (a, x, _) ->
        let key = "f" ^ string_of_int (1 + List.length (List.filter (fun (b, bx, _) -> b <> a && bx < x) hard_buttons)) in
        if clicked (x, buttons_y) 45. || pressed key || pressed (String.uppercase_ascii key) then a else app)
      m.app hard_buttons
  in
  let i : Palm.input =
    { tap = (if press then Palm.of_screen p else None);
      typed = k.typed;
      enter = k.kenter && not m.before.kenter;
      backspace = k.kbackspace && not m.before.kbackspace;
      tab = pressed "Tab";
      up = (k.kup && not m.before.kup) || clicked rocker_up 22.;
      down = (k.kdown && not m.before.kdown) || clicked rocker_down 22.;
      today = today computer }
  in
  let m = { m with app } in
  let m =
    match m.app with
    | Date_book -> let data, date_book = Pim_date_book.update i m.data m.date_book in { m with data; date_book }
    | Address -> let data, address = Pim_address.update i m.data m.address in { m with data; address }
    | Todo -> let data, todo = Pim_todo.update i m.data m.todo in { m with data; todo }
    | Memo -> let data, memo = Pim_memo.update i m.data m.memo in { m with data; memo }
  in
  (* no Save: what changed is kept, now *)
  if m.data <> m.saved then Playground_platform.store caps name (Saved.to_string ~magic m.data);
  { m with saved = m.data; before = k; was_down = mo.mdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let case = rgb 64 68 72
let silk = rgb 200 204 196

let view (computer : computer) (m : model) : shape list =
  let time = match computer.time with Time t -> t in
  let cx, cy = Palm.center in
  let lcd = Palm.size *. Palm.dots in
  let screen =
    match m.app with
    | Date_book -> Pim_date_book.view ~time m.data m.date_book
    | Address -> Pim_address.view ~time m.data m.address
    | Todo -> Pim_todo.view ~time ~today:(today computer) m.data m.todo
    | Memo -> Pim_memo.view ~time m.data m.memo
  in
  let button (a, x, label) =
    [ circle (if a = m.app then rgb 120 126 132 else rgb 90 95 100) 42. |> move x buttons_y;
      circle (rgb 50 54 58) 36. |> move x buttons_y;
      words silk label |> scale 1.4 |> move x (buttons_y -. 62.) ]
  in
  [ rectangle (rgb 220 220 214) computer.screen.width computer.screen.height;
    (* the case, and the glass *)
    rectangle case 780. 980.;
    (* the glass: the screen, and under it the Graffiti area *)
    rectangle (rgb 40 42 44) (lcd +. 40.) (lcd +. 115.) |> move cx (cy -. 37.5);
    rectangle Palm.paper lcd lcd |> move cx cy ]
  @ screen
  @ (let graffiti = cy -. (lcd /. 2.) -. 50. in
     [ (* silk-screened: letters on the left, numbers on the right *)
       rectangle silk 440. 70. |> move 0. graffiti;
       rectangle (rgb 40 42 44) 2. 70. |> move 40. graffiti;
       words (rgb 90 95 100) "abc" |> scale 2. |> move (-100.) graffiti;
       words (rgb 90 95 100) "123" |> scale 2. |> move 140. graffiti ])
  @ [
      (* the rocker *)
      rectangle (rgb 90 95 100) 60. 40. |> move (fst rocker_up) (snd rocker_up);
      rectangle (rgb 90 95 100) 60. 40. |> move (fst rocker_down) (snd rocker_down);
      triangle silk 10. |> move (fst rocker_up) (snd rocker_up);
      triangle silk 10. |> rotate 180. |> move (fst rocker_down) (snd rocker_down);
      words silk "TinyPalmPilot" |> scale 1.8 |> move 0. 467. ]
  @ List.concat_map button hard_buttons

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_in ; Cap.open_out >)))
