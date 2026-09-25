(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_line_discipline.mli *)

let show (e : Line_discipline.event) : string =
  match e with
  | Line s -> "Line " ^ s
  | Key k -> "Key " ^ String.escaped k
  | Interrupt -> "Interrupt"
  | End_of_file -> "End_of_file"

let events (evs : Line_discipline.event list) : string list = List.map show evs

(* keys typed from a fresh tty: its echo, what the program reads, and
   what is left pending *)
let typed ?(tty = Line_discipline.create ()) (keys : string) : string * string list * string =
  let tty, echo, evs = Line_discipline.input tty keys in
  (echo, events evs, Line_discipline.pending tty)

let tests =
  Testo.categorize "Line_discipline"
    [
      Testo.create "the worked example: h e l x DEL l o CR" (fun () ->
          let echo, evs, pending = typed "helx\x7flo\r" in
          Alcotest.(check string) "echo" "helx\b \blo\r\n" echo;
          Alcotest.(check (list string)) "read" [ "Line hello" ] evs;
          Alcotest.(check string) "nothing pending" "" pending;
          let screen = Vt.feed (Vt.create ~rows:2 ~cols:10) echo in
          Alcotest.(check (list string)) "the screen" [ "hello"; "" ] (Vt.text screen);
          Alcotest.(check (pair int int)) "the cursor" (1, 0) (Vt.cursor screen));
      Testo.create "nothing is read before Enter" (fun () ->
          let _, evs, pending = typed "abc" in
          Alcotest.(check (list string)) "read" [] evs;
          Alcotest.(check string) "pending" "abc" pending);
      Testo.create "two lines in one read" (fun () ->
          let _, evs, _ = typed "yes\rno\n" in
          Alcotest.(check (list string)) "read" [ "Line yes"; "Line no" ] evs);
      Testo.create "Control-U, Control-W, Backspace on nothing" (fun () ->
          let echo, _, pending = typed "abc\x15" in
          Alcotest.(check string) "U echo" "abc\b \b\b \b\b \b" echo;
          Alcotest.(check string) "U" "" pending;
          let _, _, pending = typed "go north  \x17" in
          Alcotest.(check string) "W" "go " pending;
          let echo, _, _ = typed "\x7f" in
          Alcotest.(check string) "nothing to erase" "" echo);
      Testo.create "Backspace erases a character, not a byte" (fun () ->
          let echo, _, pending = typed "caf\xC3\xA9\x7f" in
          Alcotest.(check string) "pending" "caf" pending;
          Alcotest.(check string) "one rubout" "caf\xC3\xA9\b \b" echo);
      Testo.create "Control-C and Control-D" (fun () ->
          let echo, evs, pending = typed "abc\x03" in
          Alcotest.(check string) "echo" "abc^C\r\n" echo;
          Alcotest.(check (list string)) "read" [ "Interrupt" ] evs;
          Alcotest.(check string) "the line dropped" "" pending;
          let _, evs, _ = typed "\x04" in
          Alcotest.(check (list string)) "EOF" [ "End_of_file" ] evs;
          let _, evs, _ = typed "a\x04" in
          Alcotest.(check (list string)) "not on a line" [] evs);
      Testo.create "cooked mode drops the arrows; echo off echoes nothing" (fun () ->
          let _, _, pending = typed "a\x1b[Db" in
          Alcotest.(check string) "no arrow" "ab" pending;
          let tty = Line_discipline.set_echo (Line_discipline.create ()) false in
          let echo, evs, _ = typed ~tty "secret\r" in
          Alcotest.(check string) "a password" "" echo;
          Alcotest.(check (list string)) "read" [ "Line secret" ] evs);
      Testo.create "raw mode: each key at once, unechoed, Control-C too" (fun () ->
          let tty = Line_discipline.set_mode (Line_discipline.create ()) Raw in
          let echo, evs, _ = typed ~tty "h\x1b[A\x03\xC3\xA9" in
          Alcotest.(check string) "echo" "" echo;
          Alcotest.(check (list string)) "read" [ "Key h"; "Key \\027[A"; "Key \\003"; "Key \\195\\169" ] evs);
      Testo.create "ONLCR: \\n becomes \\r\\n" (fun () ->
          Alcotest.(check string) "output" "one\r\ntwo\r\n" (Line_discipline.output "one\ntwo\n"));
      Testo.create "split_keys" (fun () ->
          Alcotest.(check (list string)) "keys"
            [ "a"; "\x1b[A"; "\x1b[5~"; "\x1bOP"; "\xE2\x94\x80"; "\x1bx"; "\x1b" ]
            (Line_discipline.split_keys "a\x1b[A\x1b[5~\x1bOP\xE2\x94\x80\x1bx\x1b"));
    ]
