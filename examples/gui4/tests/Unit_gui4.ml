(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* 7GUIs, four ways each (examples/gui4): the same scripted session into
 * the immediate, callbacks, MVC and MVU versions of a task, and after
 * every frame the same picture -- the same rectangles and texts,
 * whatever order they were painted in -- and at the end the same
 * belief about the state. gui/tests/Unit_architectures does this for a counter
 * written inline; this does it for the tasks where the architectures
 * differ the most: a two-way dependency, validation, time, and a
 * canvas with a popup, a dialog and an undo. *)

let t = Testo.create
let theme = Theme.default
let panel : Widget.box = { Widget.x = 0.; y = 0.; w = 300.; h = 420. }
let at x y : Widget.input = { Widget.no_input with mx = x; my = y }

(* a click, as the playground delivers one: the press, the hold, the
   release, and a quiet frame *)
let click (b : Widget.box) =
  let i = at b.x b.y in
  [ { i with mdown = true }; { i with mdown = true }; { i with mclick = true }; i ]

(* a right click: the menu opens on the press *)
let right_click (b : Widget.box) =
  let i = at b.x b.y in
  [ { i with mrdown = true }; { i with mrdown = true }; i; i ]

(* a key pressed and let go, and characters typed *)
let key (b : Widget.box) k = [ { (at b.x b.y) with keys = [ k ] }; at b.x b.y ]
let type_ (b : Widget.box) s = [ { (at b.x b.y) with typed = s }; at b.x b.y ]
let wait (b : Widget.box) n = List.init n (fun _ -> at b.x b.y)

(* a drag across a box, from one fraction of its width to another *)
let drag (b : Widget.box) f0 f1 =
  let x f = Widget.left b +. (f *. b.w) in
  [ { (at (x f0) b.y) with mdown = true } ]
  @ List.init 5 (fun k -> { (at (x (f0 +. ((f1 -. f0) *. float_of_int (k + 1) /. 5.))) b.y) with mdown = true })
  @ [ { (at (x f1) b.y) with mclick = true }; at (x f1) b.y ]

(* the four, run on the same frames *)
let run make frames =
  List.map
    (fun (arch, name) ->
      let r : Gui4.runner = make theme panel arch in
      let paints = List.map r.step frames in
      (name, paints, r.summary ()))
    Gui4.architectures

let check_the_four ~expect make frames =
  match run make frames with
  | (_, first, _) :: _ as all ->
      List.iter
        (fun (name, paints, summary) ->
          Alcotest.(check string) (name ^ ": what it believes") expect summary;
          List.iteri
            (fun frame (a, b) ->
              if List.sort compare a <> List.sort compare b then
                (* what one painted and the other did not, both ways *)
                let only x y = List.filter (fun p -> not (List.mem p y)) x in
                let show = function
                  | Widget.Fill (c, (b : Widget.box)) ->
                      Printf.sprintf "fill %.0f,%.0f %.0fx%.0f %s" b.x b.y b.w b.h
                        (match c with Color.Rgb (r, g, b) -> Printf.sprintf "rgb(%d,%d,%d)" r g b | Color.Hex h -> h)
                  | Widget.Text (_, (b : Widget.box), s) -> Printf.sprintf "text %S at %.0f,%.0f" s b.x b.y
                in
                Alcotest.failf "%s: frame %d is not the same picture as immediate mode's: only %s has [%s], only immediate [%s]"
                  name frame name
                  (String.concat "; " (List.map show (only b a)))
                  (String.concat "; " (List.map show (only a b))))
            (List.combine first paints))
        all
  | [] -> Alcotest.fail "no architectures"

(* where a task put a slot, found the way it does *)
let place layout slot = Gui4.places panel layout slot

let test_counter () =
  let bump = place Layout.(center (column ~gap:10. [ leaf `Count (Gui4.label_size theme "000"); leaf `Bump (Immediate.button_size theme "count") ])) `Bump in
  check_the_four ~expect:"3" Gui4Counter.make (List.concat [ click bump; click bump; click bump ])

let test_temperature () =
  let c, f =
    let l =
      Layout.(
        center
          (column ~gap:12.
             [
               leaf `C_label (Gui4.label_size theme "Celsius");
               leaf `C (Immediate.field_size theme);
               leaf `F_label (Gui4.label_size theme "Fahrenheit");
               leaf `F (Immediate.field_size theme);
             ]))
    in
    (place l `C, place l `F)
  in
  (* 20 C becomes 100 C, then 212 F is typed over by something that is
     not a number, which leaves Celsius alone *)
  let frames =
    List.concat
      [ click c; key c "End"; key c "Backspace"; key c "Backspace"; type_ c "100"; click f; key f "End"; type_ f "x"; wait f 2 ]
  in
  check_the_four ~expect:"100 C = 212.0x F" Gui4Temperature.make frames

let test_flight () =
  let l =
    Layout.(
      center
        (column ~gap:10.
           [
             leaf `Kind (Immediate.menu_size theme [ "one-way flight"; "return flight" ]);
             leaf `Out (Immediate.field_size theme);
             leaf `Back (Immediate.field_size theme);
             leaf `Book (Immediate.button_size theme "Book");
             leaf `Said (Gui4.label_size theme "booked: return, 27.3.2014");
           ]))
  in
  let kind = place l `Kind and back = place l `Back and book = place l `Book in
  let second_item = Look.menu_item theme kind 1 in
  (* return flight; its date made earlier than the outward one, so Book
     turns off (and a click on it does nothing); then later, and
     booked *)
  let frames =
    List.concat
      [
        click kind;
        click second_item;
        click back;
        key back "End";
        key back "Backspace";
        key back "Backspace";
        key back "Backspace";
        key back "Backspace";
        type_ back "2013";
        (* Book is off: the click lands on nothing, and takes the keys
           away from the field, which is clicked again *)
        click book;
        click back;
        key back "End";
        key back "Backspace";
        type_ back "5";
        click book;
      ]
  in
  check_the_four ~expect:"return flight 27.3.2014 27.3.2015 [booked: return, 27.3.2015]" Gui4Flight.make frames

let test_timer () =
  let l =
    Layout.(
      center
        (column ~gap:10.
           [
             leaf `Bar_label (Gui4.label_size theme "Elapsed Time");
             leaf `Bar (Immediate.progress_size theme);
             leaf `Figure (Gui4.label_size theme "10.0s");
             leaf `Duration_label (Gui4.label_size theme "Duration");
             leaf `Duration (Immediate.slider_size theme);
             leaf `Reset (Immediate.button_size theme "Reset");
           ]))
  in
  let duration = place l `Duration and reset = place l `Reset in
  (* two seconds pass, the duration is dragged down, Reset, and one
     more second *)
  let frames = List.concat [ wait reset 120; drag duration 0.33 0.1; click reset; wait reset 60 ] in
  let r : Gui4.runner = Gui4Timer.make theme panel Immediate in
  List.iter (fun i -> ignore (r.step i)) frames;
  check_the_four ~expect:(r.summary ()) Gui4Timer.make frames;
  Alcotest.(check bool) ("the clock was reset: " ^ r.summary ()) true (String.sub (r.summary ()) 0 4 = "1.0s")

let test_circles () =
  let l =
    Layout.(
      center
        (column ~gap:10.
           [
             row ~gap:10. [ leaf `Undo (Immediate.button_size theme "Undo"); leaf `Redo (Immediate.button_size theme "Redo") ];
             leaf `Canvas (220., 170.);
             leaf `Said (Gui4.label_size theme "Diameter at (-000, -000)");
             leaf `Diameter (Immediate.slider_size theme);
             leaf `Close (Immediate.button_size theme "Close");
           ]))
  in
  let canvas = place l `Canvas and undo = place l `Undo and diameter = place l `Diameter and close = place l `Close in
  let spot dx dy : Widget.box = { Widget.x = canvas.x +. dx; y = canvas.y +. dy; w = 1.; h = 1. } in
  let a = spot (-50.) 0. and b = spot 50. 20. in
  let item = Look.menu_item theme (Look.context_box theme (a.x, a.y) [ "Adjust diameter..." ]) 0 in
  (* two circles; the first right-clicked, adjusted with a long drag,
     and the dialog closed: three steps of history, not three plus a
     step per frame of the drag *)
  let adjusted = List.concat [ click a; click b; right_click a; click item; drag diameter 0.1 0.6; click close; wait a 2 ] in
  let r : Gui4.runner = Gui4Circles.make theme panel Immediate in
  List.iter (fun i -> ignore (r.step i)) adjusted;
  check_the_four ~expect:(r.summary ()) Gui4Circles.make adjusted;
  let s = r.summary () in
  Alcotest.(check bool) ("adjusted, one step: " ^ s) true (String.length s > 0 && String.sub s 0 10 <> "(-50,0,30)" && (let n = String.length "undo 3, redo 0" in let rec has i = i + n <= String.length s && (String.sub s i n = "undo 3, redo 0" || has (i + 1)) in has 0));
  (* and Undo: the whole adjustment taken back at once *)
  let undone = adjusted @ click undo @ wait a 2 in
  check_the_four ~expect:"(-50,0,30) (50,20,30); undo 2, redo 1" Gui4Circles.make undone

let tests =
  [
    t "7GUIs counter: four ways, one picture" test_counter;
    t "7GUIs temperature: four ways, one picture" test_temperature;
    t "7GUIs flight booker: four ways, one picture" test_flight;
    t "7GUIs timer: four ways, one picture" test_timer;
    t "7GUIs circle drawer: four ways, one picture" test_circles;
  ]
