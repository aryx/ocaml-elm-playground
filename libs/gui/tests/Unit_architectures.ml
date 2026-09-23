(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The four architectures, checked against each other: the same
 * counter, the same clicks, and -- the test that keeps the comparison
 * honest -- the *same paint*, frame by frame.
 *
 * If immediate mode, callbacks, MVC and MVU disagree about a single
 * rectangle, then examples/GuiFourWays.ml is comparing drawings
 * rather than wiring, and every claim made about them in notes_gui.md
 * section 4 is about the wrong thing. *)

let t = Testo.create
let theme = Theme.default
let count_box : Widget.box = { Widget.x = 0.; y = 50.; w = 200.; h = 36. }
let bump_box : Widget.box = { Widget.x = 0.; y = 0.; w = 200.; h = 36. }

let input ?(mdown = false) ?(mclick = false) x y : Widget.input =
  { Widget.no_input with mx = x; my = y; mdown; mclick }

(* a click on the button, as the playground delivers one: the press,
 * the hold, the release *)
let click_the_button =
  [ input ~mdown:true 0. 0.; input ~mdown:true 0. 0.; input ~mclick:true 0. 0.; input 0. 0. ]

let three_clicks = List.concat [ click_the_button; click_the_button; click_the_button ]

(* --- the same counter, four times ---------------------------------- *)

(* The button is asked for *before* the label, so that the label of
   this frame shows the count after this frame's click. In immediate
   mode that ordering is the program's to choose and plain to see; in
   the retained three it is decided for you, by when the callback
   runs. It is the only thing the four would otherwise disagree
   about, and it is not an architectural difference but a line of
   code. *)
let by_immediate frames =
  let ui = ref Immediate.empty and count = ref 0 in
  let paints =
    frames
    |> List.map (fun i ->
           let u = Immediate.frame i !ui in
           let u, clicked = Immediate.button u bump_box "count" in
           if clicked then incr count;
           let u = Immediate.label u count_box (string_of_int !count) in
           ui := u;
           Immediate.paint u)
  in
  (!count, paints)

let by_callbacks frames =
  let count = ref 0 in
  let shown = Retained.label count_box "0" in
  let ui =
    Retained.window
      (Retained.group
         [
           shown;
           Retained.button bump_box "count" (fun () ->
               incr count;
               (* the two places, made equal by hand *)
               Retained.set_text shown (string_of_int !count));
         ])
  in
  let paints = frames |> List.map (fun i -> Retained.handle i ui; Retained.paint theme ui) in
  (!count, paints)

let by_mvc frames =
  let model = Mvc.create 0 in
  let shown = Retained.label count_box "0" in
  let ui =
    Retained.window
      (Retained.group
         [ shown; Retained.button bump_box "count" (fun () -> Mvc.change model (fun n -> n + 1)) ])
  in
  Mvc.on_change model (fun () -> Retained.set_text shown (string_of_int (Mvc.get model)));
  let paints = frames |> List.map (fun i -> Retained.handle i ui; Retained.paint theme ui) in
  (Mvc.get model, paints)

type msg = Bumped

let by_mvu frames =
  let model = ref 0 and state = ref Mvu.empty in
  let view n =
    Mvu.group [ Mvu.label count_box (string_of_int n); Mvu.button bump_box "count" Bumped ]
  in
  let update Bumped n = n + 1 in
  let paints =
    frames
    |> List.map (fun i ->
           let st, m, paint = Mvu.step theme i !state ~view ~update !model in
           state := st;
           model := m;
           paint)
  in
  (!model, paints)

(* --- and the comparison -------------------------------------------- *)

let test_all_four_count_the_same () =
  let counts =
    [ by_immediate; by_callbacks; by_mvc; by_mvu ] |> List.map (fun f -> fst (f three_clicks))
  in
  Alcotest.(check (list int)) "three clicks, three counts" [ 3; 3; 3; 3 ] counts

(* the strong one: not just the same answer, the same picture *)
let test_all_four_paint_the_same () =
  let paints =
    [ by_immediate; by_callbacks; by_mvc; by_mvu ] |> List.map (fun f -> snd (f three_clicks))
  in
  let names = [ "immediate"; "callbacks"; "MVC"; "MVU" ] in
  match paints with
  | first :: rest ->
      List.iteri
        (fun i other ->
          List.iteri
            (fun frame (a, b) ->
              Alcotest.(check int)
                (Printf.sprintf "%s: frame %d paints as many things" (List.nth names (i + 1)) frame)
                (List.length a) (List.length b);
              (* the same shapes, whatever order they were asked for
                 in: nothing in a counter overlaps, so the order is
                 the tree's or the program's and means nothing here *)
              Alcotest.(check bool)
                (Printf.sprintf "%s: frame %d is the same picture" (List.nth names (i + 1)) frame)
                true
                (List.sort compare a = List.sort compare b))
            (List.combine first other))
        rest
  | [] -> Alcotest.fail "no architectures"

(* The failure mode of callbacks, written down as a test rather than
 * as an opinion: the count and what the person reads are two places,
 * and nothing but the callback keeps them equal. Change one without
 * the other -- which is what forgetting a line in one handler does --
 * and the screen says something the program does not believe. *)
let test_callbacks_can_go_out_of_sync () =
  let count = ref 0 in
  let shown = Retained.label count_box "0" in
  let ui =
    Retained.window
      (Retained.group
         [ shown; Retained.button bump_box "count" (fun () -> incr count (* and nothing else *)) ])
  in
  List.iter (fun i -> Retained.handle i ui) click_the_button;
  Alcotest.(check int) "the program believes" 1 !count;
  Alcotest.(check string) "and the screen says" "0" (Retained.text shown)

(* MVC's own cost, as a number: every change wakes every view, whether
 * or not it cares *)
let test_mvc_tells_every_view () =
  let model = Mvc.create 0 in
  let woken = ref 0 in
  Mvc.on_change model (fun () -> incr woken);
  Mvc.on_change model (fun () -> incr woken);
  Mvc.change model (fun n -> n + 1);
  Alcotest.(check int) "both views, one change" 2 !woken;
  Alcotest.(check int) "and the model counted it" 2 (Mvc.notifications model)

let tests =
  [
    t "the four architectures count the same" test_all_four_count_the_same;
    t "the four architectures paint the same" test_all_four_paint_the_same;
    t "callbacks can go out of sync" test_callbacks_can_go_out_of_sync;
    t "MVC tells every view, every time" test_mvc_tells_every_view;
  ]
