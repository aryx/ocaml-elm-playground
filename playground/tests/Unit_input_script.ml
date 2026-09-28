(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/platforms/native_common/Input_script *)

let t = Testo.create

let example () =
  match Input_script.parse "right:1-60,space:30,up:40-45,space:50" with
  | Ok s -> s
  | Error msg -> Alcotest.fail msg

let test_down () =
  let s = example () in
  Alcotest.(check (list string)) "frame 30" [ "ArrowRight"; "space" ] (Input_script.down s 30);
  Alcotest.(check (list string)) "frame 61" [] (Input_script.down s 61);
  Alcotest.(check (list string)) "frame 50" [ "ArrowRight"; "space" ] (Input_script.down s 50)

let test_changes () =
  let s = example () in
  Alcotest.(check (list (pair string bool))) "frame 1" [ ("ArrowRight", true) ] (Input_script.changes s 1);
  Alcotest.(check (list (pair string bool))) "frame 31" [ ("space", false) ] (Input_script.changes s 31);
  Alcotest.(check (list (pair string bool))) "frame 32: nothing" [] (Input_script.changes s 32);
  Alcotest.(check (list (pair string bool))) "frame 61" [ ("ArrowRight", false) ] (Input_script.changes s 61)

let test_errors () =
  let bad s = match Input_script.parse s with Ok _ -> false | Error _ -> true in
  Alcotest.(check (list bool)) "malformed" [ true; true; true; false; true; true ]
    (List.map bad [ "right"; "right:a"; "right:9-3"; ""; "at(3):1"; "at(1;2)" ])

(* the mouse, which an application needs and a game rarely does: where
 * the pointer is, and the two buttons *)
let mouse_example () =
  match Input_script.parse "at(-150;100):1-8,click:5,at(60;40):9-20,rclick:12" with
  | Ok s -> s
  | Error msg -> Alcotest.fail msg

let test_mouse () =
  let s = mouse_example () in
  let point = Alcotest.(option (pair (float 1e-9) (float 1e-9))) in
  Alcotest.check point "frame 3" (Some (-150., 100.)) (Input_script.mouse s 3);
  (* a later entry wins where two cover the same frame, so the
     pointer can be moved over a stretch *)
  Alcotest.check point "frame 9" (Some (60., 40.)) (Input_script.mouse s 9);
  Alcotest.check point "frame 30: the script says nothing" None (Input_script.mouse s 30)

let test_buttons () =
  let s = mouse_example () in
  let changes = Alcotest.(list (pair bool bool)) in
  (* a click at frame 5 is the button down during 5 and up at 6,
     which is what makes it a click rather than a press *)
  Alcotest.check changes "frame 5: the left button goes down" [ (false, true) ] (Input_script.button_changes s 5);
  Alcotest.check changes "frame 6: and up" [ (false, false) ] (Input_script.button_changes s 6);
  Alcotest.check changes "frame 12: the right one" [ (true, true) ] (Input_script.button_changes s 12);
  Alcotest.check changes "frame 30: nothing" [] (Input_script.button_changes s 30)

(* characters, which are not keys: all of them in the frame they are
 * typed at, and nothing at the others *)
let test_typing () =
  match Input_script.parse "type(edit):30,type(!):31,right:31" with
  | Error msg -> Alcotest.fail msg
  | Ok s ->
      Alcotest.(check string) "frame 30" "edit" (Input_script.typed s 30);
      Alcotest.(check string) "frame 31" "!" (Input_script.typed s 31);
      Alcotest.(check string) "frame 32" "" (Input_script.typed s 32);
      Alcotest.(check (list string)) "and the keys beside them" [ "ArrowRight" ] (Input_script.down s 31)

let tests =
  Testo.categorize "Input_script"
    [
      t "down" test_down;
      t "changes" test_changes;
      t "errors" test_errors;
      t "where the pointer is" test_mouse;
      t "the buttons going down and up" test_buttons;
      t "typing characters" test_typing;
    ]
