(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gui/Immediate: what a click is, who has the mouse, and what a
 * slider's knob means -- the mouse logic that a hand-placed [words]
 * never has, and that no golden frame can check (a golden frame is one
 * moment; these are all about *sequences* of frames). *)

let t = Testo.create
let button : Widget.box = { Widget.x = 0.; y = 100.; w = 200.; h = 40. }
let slider : Widget.box = { Widget.x = 0.; y = 0.; w = 220.; h = 36. }

let input ?(mdown = false) ?(mclick = false) x y : Widget.input =
  { Widget.no_input with mx = x; my = y; mdown; mclick }

(* the playground's own frames: a press is [mdown], a release is
 * [mclick] with [mdown] already false (see Playground.mli's mouse) *)
let press x y = input ~mdown:true x y
let release x y = input ~mclick:true x y
let hover x y = input x y

(* run [frames] over a button, collecting its answer each frame *)
let clicks_of frames =
  List.fold_left
    (fun (ui, answers) i ->
      let ui, clicked = Immediate.button (Immediate.frame i ui) button "ok" in
      (ui, answers @ [ clicked ]))
    (Immediate.empty, []) frames
  |> snd

(* the .mli's table: pressed and released inside, one click, once *)
let test_press_and_release_inside () =
  let answers = clicks_of [ hover 0. 100.; press 0. 100.; release 0. 100.; hover 0. 100. ] in
  Alcotest.(check (list bool))
    "the click is the release, and lasts one frame"
    [ false; false; true; false ] answers

(* changing your mind: every toolkit since the Macintosh lets you *)
let test_release_outside_is_not_a_click () =
  let answers = clicks_of [ press 0. 100.; hover 0. 300.; release 0. 300.; hover 0. 300. ] in
  Alcotest.(check (list bool)) "nothing happened" [ false; false; false; false ] answers

(* and the other way round: a press on the background that ends on the
 * button is not a click on the button either *)
let test_press_outside_is_not_a_click () =
  (* pressed on the background, dragged onto the button, released *)
  let answers = clicks_of [ press 0. 300.; press 0. 100.; release 0. 100. ] in
  Alcotest.(check (list bool)) "nothing happened" [ false; false; false ] answers

(* a click too fast to be seen pressed: down and up between two
 * updates, so the toolkit sees only the release, with nothing held *)
let test_click_shorter_than_a_frame () =
  let answers = clicks_of [ hover 0. 100.; release 0. 100. ] in
  Alcotest.(check (list bool)) "counted, not lost" [ false; true ] answers

(* the id is the rectangle, not the label: two buttons called "ok" *)
let test_two_buttons_same_label () =
  let other : Widget.box = { button with y = -100. } in
  let ui = Immediate.frame (press 0. 100.) Immediate.empty in
  let ui, _ = Immediate.button ui button "ok" in
  let ui, _ = Immediate.button ui other "ok" in
  let ui = Immediate.frame (release 0. 100.) ui in
  let ui, first = Immediate.button ui button "ok" in
  let _ui, second = Immediate.button ui other "ok" in
  Alcotest.(check bool) "the one that was pressed" true first;
  Alcotest.(check bool) "not its namesake elsewhere" false second

let test_checkbox_toggles_on_the_release () =
  let box : Widget.box = { Widget.x = 0.; y = 0.; w = 200.; h = 36. } in
  let ui, v = Immediate.checkbox (Immediate.frame (press 0. 0.) Immediate.empty) box "sound" false in
  Alcotest.(check bool) "not yet: a press is not a click" false v;
  let ui, v = Immediate.checkbox (Immediate.frame (release 0. 0.) ui) box "sound" v in
  Alcotest.(check bool) "now" true v;
  let _ui, v = Immediate.checkbox (Immediate.frame (hover 0. 0.) ui) box "sound" v in
  Alcotest.(check bool) "and it stays" true v

(* the .mli's worked example: 220 wide, an 18-wide knob, so the knob's
 * center travels over 202 pixels, from x = -101 to x = 101 *)
let test_slider_maps_the_mouse () =
  let drag x =
    let ui = Immediate.frame (press x 0.) Immediate.empty in
    snd (Immediate.slider ui slider ~from:0. ~to_:100. 50.)
  in
  Alcotest.(check (float 1e-6)) "the left end" 0. (drag (-101.));
  Alcotest.(check (float 1e-6)) "the middle" 50. (drag 0.);
  Alcotest.(check (float 1e-6)) "the right end" 100. (drag 101.);
  (* a press that is not on the slider is not the slider's *)
  Alcotest.(check (float 1e-6)) "pressed elsewhere, unmoved" 50. (drag 400.)

(* capture: the press belongs to the slider until it is released, even
 * when the mouse wanders off -- without this, dragging is infuriating *)
let test_slider_keeps_the_mouse () =
  let ui = Immediate.frame (press (-101.) 0.) Immediate.empty in
  let ui, v = Immediate.slider ui slider ~from:0. ~to_:100. 50. in
  Alcotest.(check (float 1e-6)) "grabbed at the left end" 0. v;
  (* the mouse leaves the slider, still held *)
  let ui, v = Immediate.slider (Immediate.frame (press 50. (-400.)) ui) slider ~from:0. ~to_:100. v in
  Alcotest.(check (float 1e-6)) "still ours, and still following x" 74.752475 v;
  (* dragged past its right end: the value stops at [to_] *)
  let ui, v = Immediate.slider (Immediate.frame (press 900. (-400.)) ui) slider ~from:0. ~to_:100. v in
  Alcotest.(check (float 1e-6)) "clamped" 100. v;
  (* released far away: the value stays where the drag left it *)
  let _ui, v = Immediate.slider (Immediate.frame (hover 900. (-400.)) ui) slider ~from:0. ~to_:100. v in
  Alcotest.(check (float 1e-6)) "let go" 100. v

(* a widget that is not touched must not become hot because another
 * one was pressed: the capture is one widget's, not the toolkit's *)
let test_paint_is_one_frame () =
  let ui = Immediate.frame (hover 0. 0.) Immediate.empty in
  let ui, _ = Immediate.button ui button "ok" in
  (* a fill, four bars of frame, a label *)
  Alcotest.(check int) "what a button draws" 6 (List.length (Immediate.paint ui));
  let ui = Immediate.frame (hover 0. 0.) ui in
  Alcotest.(check int) "a new frame starts empty" 0 (List.length (Immediate.paint ui))

(* a list box: a click on a row selects that row, a click below the
   last item keeps the selection, and the selection is the caller's *)
let test_list_selects_a_row () =
  let list : Widget.box = { Widget.x = 0.; y = 0.; w = 200.; h = 180. } in
  let items = [ "Emil, Hans"; "Mustermann, Max"; "Tisch, Roman" ] in
  let row i = (Look.list_row Theme.default list i).y in
  let run frames selected =
    List.fold_left
      (fun (ui, sel) i ->
        let ui = Immediate.frame i ui in
        let ui, sel = Immediate.list ui list items sel in
        (ui, sel))
      (Immediate.empty, selected) frames
    |> snd
  in
  let click y = [ press 0. y; release 0. y; hover 0. y ] in
  Alcotest.(check (option int)) "the second row" (Some 1) (run (click (row 1)) None);
  Alcotest.(check (option int)) "below the last item: unchanged" (Some 1) (run (click (row 4)) (Some 1));
  Alcotest.(check (option int)) "no click, no change" None (run [ hover 0. (row 0) ] None)

(* two fields share one caret, and only the focused one moves it: the
   bug examples/gui4/tests/Unit_gui4 found, where a field without the keys
   and a shorter text pulled the caret of the one being typed in *)
let test_an_unfocused_field_leaves_the_caret () =
  let short : Widget.box = { Widget.x = 0.; y = 100.; w = 200.; h = 36. } in
  let long : Widget.box = { Widget.x = 0.; y = 0.; w = 200.; h = 36. } in
  let frame i (ui, a, b) =
    let ui = Immediate.frame i ui in
    let ui, a = Immediate.field ui short a in
    let ui, b = Immediate.field ui long b in
    (ui, a, b)
  in
  let frames =
    [ press 0. 0.; release 0. 0.; hover 0. 0.; { (hover 0. 0.) with keys = [ "End" ] }; hover 0. 0.; { (hover 0. 0.) with typed = "!" }; hover 0. 0. ]
  in
  let _, _, b = List.fold_left (fun st i -> frame i st) (Immediate.empty, "ab", "abcdef") frames in
  Alcotest.(check string) "typed at the end of the long one" "abcdef!" b

let tests =
  [
    t "a click is a press and a release, both inside" test_press_and_release_inside;
    t "a press that ends outside is not a click" test_release_outside_is_not_a_click;
    t "a press that began outside is not a click" test_press_outside_is_not_a_click;
    t "a click shorter than a frame still counts" test_click_shorter_than_a_frame;
    t "a widget is told apart by its rectangle, not its label" test_two_buttons_same_label;
    t "a checkbox toggles on the release" test_checkbox_toggles_on_the_release;
    t "a slider maps the mouse onto its range" test_slider_maps_the_mouse;
    t "a slider keeps the mouse while dragged off it" test_slider_keeps_the_mouse;
    t "the paint lasts one frame" test_paint_is_one_frame;
    t "a list selects the row clicked" test_list_selects_a_row;
    t "an unfocused field leaves the caret alone" test_an_unfocused_field_leaves_the_caret;
  ]
