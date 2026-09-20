(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/Playground: the computer's transient inputs -- the
 * characters typed, the wheel's notches and the double click (see
 * docs/claude_notes/plans/plan_gui_teaching.md, phase 0).
 *
 * The property that matters, and the one a backend can break without
 * anyone noticing: each of the three is *set by an event, seen by
 * exactly one update, then cleared*. Too early and a text field misses
 * a letter; too late and it gets it twice. *)

let t = Testo.create

(* an app whose update records what each frame's computer showed it *)
let recording () =
  let seen = ref [] in
  let update (computer : Playground.computer) () =
    seen :=
      (computer.keyboard.typed, computer.mouse.mwheel, computer.mouse.mdouble) :: !seen
  in
  let app = Playground.game (fun _ _ -> []) update () in
  let model = fst (app.init []) in
  (seen, app, model)

let drive (app : ('m, Playground.msg) Playground.app) (msgs : Playground.msg list) m =
  List.fold_left (fun m msg -> fst (app.update msg m)) m msgs

let tick n = Playground.Tick (Time.millis_to_posix n)

let test_typed_accumulates_then_clears () =
  let seen, app, model = recording () in
  let model =
    drive app [ Playground.Typed "h"; Playground.Typed "i"; tick 16 ] model
  in
  let _ = drive app [ tick 32 ] model in
  (* newest first: the second frame saw nothing, the first saw "hi" *)
  match !seen with
  | [ (second, _, _); (first, _, _) ] ->
      Alcotest.(check string) "the frame after the keys sees them" "hi" first;
      Alcotest.(check string) "and the next frame does not" "" second
  | l -> Alcotest.failf "expected two frames, got %d" (List.length l)

let test_wheel_accumulates_then_clears () =
  let seen, app, model = recording () in
  let model =
    drive app [ Playground.MouseWheel 1.; Playground.MouseWheel 2.; tick 16 ] model
  in
  let _ = drive app [ Playground.MouseWheel (-0.5); tick 32 ] model in
  match !seen with
  | [ (_, second, _); (_, first, _) ] ->
      (* the notches of one frame add up, in one number *)
      Alcotest.(check (float 1e-9)) "three notches up" 3. first;
      Alcotest.(check (float 1e-9)) "then half a notch down" (-0.5) second
  | l -> Alcotest.failf "expected two frames, got %d" (List.length l)

let test_double_click_lasts_one_frame () =
  let seen, app, model = recording () in
  let model = drive app [ Playground.MouseDouble; tick 16 ] model in
  let _ = drive app [ tick 32 ] model in
  match !seen with
  | [ (_, _, second); (_, _, first) ] ->
      Alcotest.(check bool) "the frame of the double click" true first;
      Alcotest.(check bool) "the frame after it" false second
  | l -> Alcotest.failf "expected two frames, got %d" (List.length l)

(* a key name is not a character: shift is in [keys], never in [typed],
 * and "A" reaches a text field only through [typed] *)
let test_keys_and_typed_are_different_questions () =
  let seen, app, model = recording () in
  let _ =
    drive app
      [ Playground.KeyChanged (true, "shift"); Playground.Typed "A"; tick 16 ]
      model
  in
  match !seen with
  | [ (typed, _, _) ] -> Alcotest.(check string) "the character, not the key" "A" typed
  | l -> Alcotest.failf "expected one frame, got %d" (List.length l)

let tests =
  [
    t "typed accumulates over a frame, then clears" test_typed_accumulates_then_clears;
    t "the wheel's notches add up over a frame, then clear" test_wheel_accumulates_then_clears;
    t "a double click lasts exactly one frame" test_double_click_lasts_one_frame;
    t "a key name is not a character" test_keys_and_typed_are_different_questions;
  ]
