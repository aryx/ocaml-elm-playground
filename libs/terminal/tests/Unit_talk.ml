(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Talk

(* See Unit_talk.mli *)

(* the .mli's example *)
let hello : unit talk =
  let* () = print "WHAT IS YOUR NAME? " in
  let* name = read_line in
  print ("HELLO, " ^ name ^ "\n")

let dice : unit talk =
  let* a = random 6 in
  let* b = random 6 in
  print (Printf.sprintf "%d %d\n" a b)

let screen_text (m : machine) : string list = Vt.text (screen m)

let tests =
  Testo.categorize "Talk"
    [
      Testo.create "run: the name example, the answer echoed" (fun () ->
          Alcotest.(check string) "transcript" "WHAT IS YOUR NAME? BOB\nHELLO, BOB\n" (run hello [ "BOB" ]);
          Alcotest.(check string) "no answer: stops at the question" "WHAT IS YOUR NAME? " (run hello []));
      Testo.create "the same seed, the same dice; another, others" (fun () ->
          Alcotest.(check string) "twice" (run ~seed:7 dice []) (run ~seed:7 dice []);
          let all = List.init 20 (fun s -> run ~seed:s dice []) in
          Alcotest.(check bool) "not all the same" true (List.exists (( <> ) (List.hd all)) all));
      Testo.create "a loop reads like BASIC: ask until the right answer" (fun () ->
          let rec guess () =
            let* g = ask "? " in
            if g = "42" then print "RIGHT\n" else guess ()
          in
          Alcotest.(check string) "transcript" "? 1\n? 42\nRIGHT\n" (run (guess ()) [ "1"; "42"; "never read" ]));
      Testo.create "the machine: a line typed, a typo fixed, on the screen" (fun () ->
          let m = start ~seed:1 ~rows:3 ~cols:30 hello in
          Alcotest.(check bool) "waiting" true (reading m);
          let m = input m "BOX\x7fB" in
          Alcotest.(check (list string)) "typing" [ "WHAT IS YOUR NAME? BOB"; ""; "" ] (screen_text m);
          let m = input m "\r" in
          Alcotest.(check (list string)) "answered" [ "WHAT IS YOUR NAME? BOB"; "HELLO, BOB"; "" ] (screen_text m);
          Alcotest.(check bool) "the end" true (finished m));
      Testo.create "a key, in raw mode: no Enter, no echo" (fun () ->
          let p =
            let* k = read_key in
            print ("[" ^ k ^ "]")
          in
          let m = input (start ~seed:1 ~rows:1 ~cols:10 p) "y" in
          Alcotest.(check (list string)) "screen" [ "[y]" ] (screen_text m));
      Testo.create "Control-C ends the program" (fun () ->
          let m = input (start ~seed:1 ~rows:2 ~cols:30 hello) "BO\x03" in
          Alcotest.(check bool) "the end" true (finished m);
          Alcotest.(check (list string)) "screen" [ "WHAT IS YOUR NAME? BO^C"; "" ] (screen_text m));
      Testo.create "spawn: a shell running hello, then carrying on" (fun () ->
          let shell =
            let* st = spawn hello in
            print (if st = Exited then "BACK\n" else "INTERRUPTED\n")
          in
          Alcotest.(check string) "run" "WHAT IS YOUR NAME? BOB\nHELLO, BOB\nBACK\n" (run shell [ "BOB" ]);
          let m = input (start ~seed:1 ~rows:4 ~cols:30 shell) "BOB\r" in
          Alcotest.(check (list string)) "the machine" [ "WHAT IS YOUR NAME? BOB"; "HELLO, BOB"; "BACK"; "" ] (screen_text m));
      Testo.create "Control-C interrupts the child only, the parent carries on" (fun () ->
          let rec shell () =
            let* line = ask "$ " in
            if line = "exit" then return ()
            else
              let* st = spawn hello in
              let* () = print (if st = Interrupted then "(interrupted)\n" else "") in
              shell ()
          in
          let m = start ~seed:1 ~rows:4 ~cols:30 (shell ()) in
          let m = input m "go\rBO\x03" in
          Alcotest.(check (list string)) "screen" [ "$ go"; "WHAT IS YOUR NAME? BO^C"; "(interrupted)"; "$" ] (screen_text m);
          Alcotest.(check bool) "the shell reads again" true (reading m);
          let m = input m "\x03" in
          Alcotest.(check bool) "at the shell, Control-C ends it" true (finished m));
      Testo.create "step: a program that never ends, run for a million steps" (fun () ->
          let rec forever n : unit talk =
            let* () = step in
            if n = 3 then
              let* () = print "3\n" in
              forever (n + 1)
            else forever (n + 1)
          in
          Alcotest.(check string) "run stops it" "3\n" (run (forever 0) []));
      Testo.create "step: the machine takes its steps a frame at a time" (fun () ->
          (* counts to 50,000, printing each 10,000th: more than a frame *)
          let rec count n : unit talk =
            let* () = step in
            if n > 50_000 then return ()
            else
              let* () = if n mod 10_000 = 0 then print (Printf.sprintf "%d " n) else return () in
              count (n + 1)
          in
          let m = start ~seed:1 ~rows:1 ~cols:40 (count 1) in
          Alcotest.(check bool) "not over after the first frame" false (finished m);
          let m = tick (tick (tick m 0.02) 0.02) 0.02 in
          Alcotest.(check bool) "over three frames later" true (finished m);
          Alcotest.(check (list string)) "all printed" [ "10000 20000 30000 40000 50000" ] (screen_text m));
      Testo.create "110 baud: 10 characters a second" (fun () ->
          let m = start ~baud:110 ~seed:1 ~rows:1 ~cols:30 hello in
          Alcotest.(check (list string)) "nothing yet" [ "" ] (screen_text m);
          let m = tick m 0.5 in
          Alcotest.(check (list string)) "half a second" [ "WHAT" ] (screen_text m);
          Alcotest.(check (pair int int)) "five characters" (0, 5) (Vt.cursor (screen m));
          Alcotest.(check bool) "not reading while printing" false (reading m);
          let m = tick m 10. in
          Alcotest.(check bool) "then reading" true (reading m));
    ]
