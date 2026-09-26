(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* core: Program *)

let t = Testo.create

(* alone, then as tinybox links them: the first program runs at once,
 * the next two are only recorded, and the one run is given its own
 * command line. One test, in order: collect can't be undone. *)
let test_alone_then_collected () =
  let ran = ref [] in
  let program name () = ran := (name, Program.argv ()) :: !ran in
  Program.main "Alone" (program "Alone");
  Alcotest.(check (list string)) "alone: run at once" [ "Alone" ] (List.map fst !ran);
  Alcotest.(check (array string)) "alone: the process's argv" Sys.argv (List.assoc "Alone" !ran);
  Program.collect ();
  Program.main "TinyMario" (program "TinyMario");
  Program.main "TinyWinamp" (program "TinyWinamp");
  Alcotest.(check int) "collected: nothing run" 1 (List.length !ran);
  Alcotest.(check (list string)) "in link order" [ "TinyMario"; "TinyWinamp" ] (List.map fst (Program.collected ()));
  let argv = [| "TinyWinamp"; "dir=~/Music" |] in
  Program.run "TinyWinamp" ~argv;
  Alcotest.(check (array string)) "run: the argv it was given" argv (List.assoc "TinyWinamp" !ran);
  Alcotest.check_raises "run: an unknown name" Not_found (fun () -> Program.run "TinyNothing" ~argv);
  Alcotest.check_raises "two programs of one name" (Failure "Program.main: two programs named TinyMario") (fun () ->
      Program.main "TinyMario" (program "TinyMario"))

let tests = Testo.categorize "Program" [ t "alone, then collected and run" test_alone_then_collected ]
