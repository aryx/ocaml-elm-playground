(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/native_common/Input_script *)

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
  Alcotest.(check (list bool)) "malformed" [ true; true; true; false ]
    (List.map bad [ "right"; "right:a"; "right:9-3"; "" ])

let tests =
  Testo.categorize "Input_script" [ t "down" test_down; t "changes" test_changes; t "errors" test_errors ]
