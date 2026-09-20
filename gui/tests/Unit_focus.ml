(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gui/Focus and Immediate.field: who gets the keys, and what typing
 * into a field does -- all of it sequences of frames, which is why
 * none of it can be checked by a picture. *)

let t = Testo.create
let a : Widget.id = (0., 100.)
let b : Widget.id = (0., 50.)
let c : Widget.id = (0., 0.)

(* a frame that asked for the three, in that order *)
let three () = Focus.(none |> frame |> saw a |> saw b |> saw c |> frame)

let who f =
  if Focus.has a f then "a" else if Focus.has b f then "b" else if Focus.has c f then "c" else "none"

(* the .mli's table: Tab walks the order the widgets were asked in *)
let test_tab_walks_the_asking_order () =
  let f = three () in
  Alcotest.(check string) "nobody yet: Tab takes the first" "a" (who (Focus.next f));
  Alcotest.(check string) "then the second" "b" (who (Focus.next (Focus.next f)));
  Alcotest.(check string)
    "and it wraps round" "a"
    (who (Focus.next (Focus.next (Focus.next (Focus.next f)))))

let test_shift_tab_walks_back () =
  let f = three () in
  Alcotest.(check string) "nobody yet: Shift-Tab takes the last" "c" (who (Focus.previous f));
  Alcotest.(check string)
    "from b, back to a" "a"
    (who (Focus.previous (Focus.give b f)));
  Alcotest.(check string) "and from a it wraps to c" "c" (who (Focus.previous (Focus.give a f)))

let test_a_click_gives_and_the_backdrop_takes_away () =
  let f = three () in
  Alcotest.(check string) "clicked" "b" (who (Focus.give b f));
  Alcotest.(check string) "and let go" "none" (who (Focus.clear (Focus.give b f)))

(* a widget that is not asked for any more cannot keep the keys
 * forever: Tab starts again from the first *)
let test_focus_on_a_widget_that_went_away () =
  let f = Focus.(none |> frame |> saw a |> saw b |> frame |> give c) in
  Alcotest.(check string) "back to the first" "a" (who (Focus.next f))

(* --- the field --------------------------------------------------- *)

let box : Widget.box = { Widget.x = 0.; y = 0.; w = 220.; h = 36. }

let input ?(mdown = false) ?(mclick = false) ?(typed = "") ?(keys = []) x y : Widget.input =
  { Widget.no_input with mx = x; my = y; mdown; mclick; typed; keys }

(* type [what] into a field that starts empty, one frame per item *)
let typing what =
  List.fold_left
    (fun (ui, text) i ->
      let ui, text = Immediate.field (Immediate.frame i ui) box text in
      (ui, text))
    (Immediate.empty, "")
    what

(* a field takes what the platform says was typed, wherever it came
 * from -- and nothing at all until it has the keys *)
let test_typing_needs_the_focus () =
  let _, text = typing [ input ~typed:"h" 0. 0.; input ~typed:"i" 0. 0. ] in
  Alcotest.(check string) "no focus, no text" "" text;
  let _, text =
    typing
      [
        input ~mdown:true 0. 0.;
        input ~mclick:true 0. 0. (* the click gives it the keys *);
        input ~typed:"h" 0. 0.;
        input ~typed:"i" 0. 0.;
      ]
  in
  Alcotest.(check string) "then it types" "hi" text

let clicked_field () = [ input ~mdown:true 0. 0.; input ~mclick:true 0. 0. ]

let test_backspace_and_the_arrows () =
  let _, text =
    typing (clicked_field () @ [ input ~typed:"abc" 0. 0.; input ~keys:[ "Backspace" ] 0. 0. ])
  in
  Alcotest.(check string) "the last character goes" "ab" text;
  (* left, left, then type: the caret is where it was put. The empty
     frames matter -- a key held across two frames went down once *)
  let _, text =
    typing
      (clicked_field ()
      @ [
          input ~typed:"abc" 0. 0.;
          input ~keys:[ "ArrowLeft" ] 0. 0.;
          input 0. 0.;
          input ~keys:[ "ArrowLeft" ] 0. 0.;
          input ~typed:"X" 0. 0.;
        ])
  in
  Alcotest.(check string) "typed in the middle" "aXbc" text

(* holding a key is not pressing it again: the toolkit works on edges,
 * so a key held across frames acts once (and a field has no repeat) *)
let test_a_held_key_acts_once () =
  let _, text =
    typing
      (clicked_field ()
      @ [
          input ~typed:"abc" 0. 0.;
          input ~keys:[ "Backspace" ] 0. 0.;
          input ~keys:[ "Backspace" ] 0. 0. (* still held *);
          input ~keys:[ "Backspace" ] 0. 0.;
        ])
  in
  Alcotest.(check string) "one deletion, not three" "ab" text

(* a character is not a byte: backspace takes the whole of an accented
 * letter, which is two bytes of UTF-8 *)
let test_backspace_eats_a_whole_character () =
  let _, text =
    typing (clicked_field () @ [ input ~typed:"caf\xc3\xa9" 0. 0.; input ~keys:[ "Backspace" ] 0. 0. ])
  in
  Alcotest.(check string) "the e acute goes, both its bytes" "caf" text

(* Tab moves the keys from one field to the next, in the order the
 * fields were asked for *)
let test_tab_moves_between_two_fields () =
  let first : Widget.box = { box with y = 50. } in
  let second : Widget.box = { box with y = -50. } in
  (* the two texts are the caller's, so a frame carries them through *)
  let two (ui, one, other) i =
    let ui = Immediate.frame i ui in
    let ui, one = Immediate.field ui first one in
    let ui, other = Immediate.field ui second other in
    (ui, one, other)
  in
  (* click the first, type, Tab, type *)
  let state = (Immediate.empty, "", "") in
  let state = two state (input ~mdown:true 0. 50.) in
  let state = two state (input ~mclick:true 0. 50.) in
  let state = two state (input ~typed:"a" 0. 50.) in
  let state = two state (input ~keys:[ "Tab" ] 0. 50.) in
  let _, one, other = two state (input ~typed:"b" 0. 50.) in
  Alcotest.(check string) "what was typed before Tab" "a" one;
  Alcotest.(check string) "and what was typed after it" "b" other

let tests =
  [
    t "Tab walks the order the widgets were asked in" test_tab_walks_the_asking_order;
    t "Shift-Tab walks it backwards" test_shift_tab_walks_back;
    t "a click gives the keys, the backdrop takes them away"
      test_a_click_gives_and_the_backdrop_takes_away;
    t "a widget that went away does not keep the keys" test_focus_on_a_widget_that_went_away;
    t "a field types only when it has the keys" test_typing_needs_the_focus;
    t "backspace, and the arrows moving the caret" test_backspace_and_the_arrows;
    t "a held key acts once" test_a_held_key_acts_once;
    t "backspace takes a whole character, not a byte" test_backspace_eats_a_whole_character;
    t "Tab moves the keys to the next field" test_tab_moves_between_two_fields;
  ]
