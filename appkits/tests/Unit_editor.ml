(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_editor.mli *)

let check = Alcotest.(check string)

let contains (s : string) (sub : string) : bool =
  let n = String.length sub in
  let rec go i = i + n <= String.length s && (String.sub s i n = sub || go (i + 1)) in
  go 0

(*****************************************************************************)
(* Gap_buffer *)
(*****************************************************************************)

let gap_tests =
  [ Testo.create "Gap_buffer: the .mli's cat, typing at the gap, moving it" (fun () ->
        let t = Gap_buffer.of_string "The cat." in
        let t = Gap_buffer.insert t 7 " sat" in
        check "inserted" "The cat sat." (Gap_buffer.to_string t);
        Alcotest.(check int) "the gap after it" 11 (Gap_buffer.gap t);
        let t = Gap_buffer.delete t 0 4 in
        check "deleted" "cat sat." (Gap_buffer.to_string t);
        Alcotest.(check int) "the gap moved there" 0 (Gap_buffer.gap t);
        check "sub" "sat" (Gap_buffer.sub t 4 7));
    Testo.create "Gap_buffer: old versions stay whole, a branch copies" (fun () ->
        let v0 = Gap_buffer.of_string "abc" in
        let v1 = Gap_buffer.insert v0 3 "d" in
        let v2 = Gap_buffer.insert v1 4 "e" in
        (* v1 again, from where v2 wrote in place: it must copy *)
        let v2' = Gap_buffer.insert v1 4 "X" in
        let v3 = Gap_buffer.insert (Gap_buffer.delete v2 0 1) 0 "Z" in
        check "v0" "abc" (Gap_buffer.to_string v0);
        check "v1" "abcd" (Gap_buffer.to_string v1);
        check "v2" "abcde" (Gap_buffer.to_string v2);
        check "v2'" "abcdX" (Gap_buffer.to_string v2');
        check "v3" "Zbcde" (Gap_buffer.to_string v3)) ]

(*****************************************************************************)
(* TinyEmacs *)
(*****************************************************************************)

let emacs = Tui_emacs.program

(* keys as Line_discipline cuts what a terminal sends *)
let type_ (m : Tui_emacs.model) (bytes : string) : Tui_emacs.model =
  List.fold_left (fun m k -> emacs.update (Key k) m) m (Line_discipline.split_keys bytes)

let emacs_with (bytes : string) = type_ emacs.init bytes

(* the text of *scratch* after its two lines of comments *)
let scratch_rest (m : Tui_emacs.model) : string =
  let t = Tui_emacs.text m in
  let intro = ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n" in
  let rec find i = if i + String.length intro > String.length t then t else if String.sub t i (String.length intro) = intro then String.sub t (i + String.length intro) (String.length t - i - String.length intro) else find (i + 1) in
  find 0

let emacs_tests =
  [ Testo.create "Emacs: it starts in *scratch*, its .emacs loaded" (fun () ->
        let m = emacs.init in
        check "buffer" "*scratch*" (Tui_emacs.buffer_name m);
        check "message" "Welcome to TinyEmacs. C-h t: the tutorial; C-x C-c: quit." (Tui_emacs.message m);
        check ".emacs's command" "Hello from your .emacs! 1 buffers." (Tui_emacs.message (emacs_with "\x03h")));
    Testo.create "Emacs: typing, and one undo for the run of it; after anything else, undo undoes the undo" (fun () ->
        let m = emacs_with "hello" in
        check "typed" "hello" (scratch_rest m);
        let m = type_ m "\x1f" in
        check "undone" "" (scratch_rest m);
        let m = type_ m "\x07\x1f" in
        check "redone" "hello" (scratch_rest m));
    Testo.create "Emacs: C-k twice kills a line and its newline as one kill; C-y yanks it" (fun () ->
        let m = emacs_with "one\rtwo\x1b<\x1b>\x10\x01" in
        let m = type_ m "\x0b\x0b" in
        check "killed" "two" (scratch_rest m);
        let m = type_ m "\x1b>\x19" in
        check "yanked" "twoone\n" (scratch_rest m));
    Testo.create "Emacs: C-j in *scratch* evaluates, C-x C-e shows, M-: asks" (fun () ->
        check "C-j" "(+ 1 2)\n3\n" (scratch_rest (emacs_with "(+ 1 2)\n"));
        check "C-x C-e" "(3 2 1)" (Tui_emacs.message (emacs_with "(reverse '(1 2 3))\x18\x05"));
        check "M-:" "\"*scratch*\"" (Tui_emacs.message (emacs_with "\x1b:(buffer-name)\r"));
        check "an error" "Symbol's function definition is void: nope" (Tui_emacs.message (emacs_with "\x1b:(nope)\r")));
    Testo.create "Emacs: M-x completes a command's name with TAB" (fun () ->
        check "what-line" "Line 4" (Tui_emacs.message (emacs_with "\x1bxwhat-li\t\r")));
    Testo.create "Emacs: C-h k reads a key, and the keymap says what it runs" (fun () ->
        check "C-f" "C-f runs forward-char: Move point N characters forward (backward if N is negative)."
          (Tui_emacs.message (emacs_with "\x08k\x06"));
        check "C-x C-f" "C-x C-f runs find-file: Edit the file FILENAME, in a buffer of its own."
          (Tui_emacs.message (emacs_with "\x08k\x18\x06"));
        check "undefined" "C-x C-z is undefined" (Tui_emacs.message (emacs_with "\x18\x1a")));
    Testo.create "Emacs: a file visited, changed and saved" (fun () ->
        let m = emacs_with "\x18\x06notes.txt\r" in
        check "visited" "notes.txt" (Tui_emacs.buffer_name m);
        let m = type_ m "Done: \x18\x13" in
        check "saved" "Wrote notes.txt" (Tui_emacs.message m);
        check "on the disk" "Done: Things to do:" (List.hd (String.split_on_char '\n' (Option.get (Tui_emacs.file m "notes.txt")))));
    Testo.create "Emacs: C-s searches as you type, C-g goes back" (fun () ->
        let m = emacs_with "\x18\x06TUTORIAL\r\x13yank" in
        let t = Tui_emacs.text m and p = Tui_emacs.point m - 1 in
        check "found" "yank" (String.sub t (p - 4) 4);
        let m = type_ m "\x07" in
        Alcotest.(check int) "back" 1 (Tui_emacs.point m));
    Testo.create "Emacs: C-u C-u C-f moves 16" (fun () ->
        let m = emacs_with "\x1b<\x15\x15\x06" in
        Alcotest.(check int) "point" 17 (Tui_emacs.point m));
    Testo.create "Emacs: the screen, its mode line" (fun () ->
        let rows = Curses.text (emacs.view (emacs_with "x")) in
        let mode = List.nth rows 22 in
        Alcotest.(check bool) mode true (String.length mode > 30 && String.sub mode 0 25 = "-UUU:**-  *scratch*      ")) ]

(*****************************************************************************)
(* TinyVi *)
(*****************************************************************************)

let vi = Tui_vi.program

let vi_type (m : Tui_vi.model) (bytes : string) : Tui_vi.model = List.fold_left (fun m k -> vi.update (Key k) m) m (Line_discipline.split_keys bytes)

(* vi on the poem, the cursor on its first line *)
let poem (bytes : string) : Tui_vi.model = vi_type vi.init (":e poem.txt\r" ^ bytes)

let first (m : Tui_vi.model) : string = List.hd (Tui_vi.lines m)
let lines = Alcotest.(list string)

let vi_tests =
  [ Testo.create "vi: the grammar -- counts multiply, operators take motions" (fun () ->
        let show = function
          | Tui_vi.Incomplete -> "incomplete"
          | Invalid -> "invalid"
          | Complete (n, cmd) -> Printf.sprintf "%s %s" (match n with Some n -> string_of_int n | None -> "-") (String.concat "" cmd)
        in
        check "d" "incomplete" (show (Tui_vi.parse_command [ "d" ]));
        check "2d3w" "6 dw" (show (Tui_vi.parse_command [ "2"; "d"; "3"; "w" ]));
        check "dd" "- dd" (show (Tui_vi.parse_command [ "d"; "d" ]));
        check "df," "- df," (show (Tui_vi.parse_command [ "d"; "f"; "," ]));
        check "10j" "10 j" (show (Tui_vi.parse_command [ "1"; "0"; "j" ]));
        check "0 is a motion" "- 0" (show (Tui_vi.parse_command [ "0" ]));
        check "dz" "invalid" (show (Tui_vi.parse_command [ "d"; "z" ])));
    Testo.create "vi: it starts on README, in normal mode" (fun () ->
        check "mode" "normal" (Tui_vi.mode vi.init);
        check "status" "\"README\" 23 lines, 1062 characters" (Tui_vi.message vi.init));
    Testo.create "vi: dw, cw, d$, x, and . repeating the change" (fun () ->
        check "dw" "woods these are I think I know." (first (poem "dw"));
        check "2dw" "these are I think I know." (first (poem "2dw"));
        check "cw" "Which woods these are I think I know." (first (poem "cwWhich\x1b"));
        check "cw then w ." "Which Which these are I think I know." (first (poem "cwWhich\x1bw."));
        check "d$" "" (first (poem "d$"));
        check "3x" "se woods these are I think I know." (first (poem "3x"));
        check "dfs" "e woods these are I think I know." (first (poem "dfs")));
    Testo.create "vi: dd, p, u, and u again redoing" (fun () ->
        let m = poem "ddp" in
        Alcotest.check lines "swapped" [ "His house is in the village though;"; "Whose woods these are I think I know." ] (List.filteri (fun i _ -> i < 2) (Tui_vi.lines m));
        let m = poem "2dd" in
        Alcotest.check lines "two gone" [ "He will not see me stopping here"; "To watch his woods fill up with snow." ] (Tui_vi.lines m);
        let m = vi_type m "u" in
        Alcotest.(check int) "undone" 4 (List.length (Tui_vi.lines m));
        let m = vi_type m "u" in
        Alcotest.(check int) "redone" 2 (List.length (Tui_vi.lines m)));
    Testo.create "vi: insert, o, J, and Escape typed with the next key" (fun () ->
        check "i" "Oh whose woods these are I think I know." (first (poem "iOh \x1b\x1bl~"));
        let m = poem "oA new line\x1b" in
        check "o" "A new line" (List.nth (Tui_vi.lines m) 1);
        check "J" "Whose woods these are I think I know. His house is in the village though;" (first (poem "J"));
        let m = poem "A!\x1bj" in
        Alcotest.(check (pair int int)) "Escape then j, as one key" (1, 34) (Tui_vi.cursor m));
    Testo.create "vi: search, and ex's :s, :%s, :N, :w, :q" (fun () ->
        let m = poem "/snow\r" in
        Alcotest.(check (pair int int)) "found" (3, 32) (Tui_vi.cursor m);
        check ":s" "Whose trees these are I think I know." (first (poem ":s/woods/trees/\r"));
        let m = poem ":%s/s/S/g\r" in
        check ":%s" "WhoSe woodS theSe are I think I know." (first m);
        check "counted" "11 substitutions" (Tui_vi.message m);
        Alcotest.(check (pair int int)) ":3" (2, 0) (Tui_vi.cursor (poem ":3\r"));
        let m = poem "dd:q\r" in
        check "q refused" "No write since last change (add ! to override)" (Tui_vi.message m);
        let m = vi_type m ":w\r" in
        check "written" "His house is in the village though;" (List.hd (String.split_on_char '\n' (Option.get (Tui_vi.file m "poem.txt"))));
        Alcotest.(check bool) "q" true (vi.over (vi_type m ":q\r"))) ]

(*****************************************************************************)
(* TinyTurboPascal *)
(*****************************************************************************)

let turbo = Tui_turbo.program

let tp_type (m : Tui_turbo.model) (bytes : string) : Tui_turbo.model = List.fold_left (fun m k -> turbo.update (Key k) m) m (Line_discipline.split_keys bytes)

(* the program run to its end: frames of a thirtieth of a second *)
let rec tp_ticks (m : Tui_turbo.model) (n : int) : Tui_turbo.model =
  if n = 0 || Tui_turbo.screen m <> "run" then m else tp_ticks (turbo.update (Tick (1. /. 30.)) m) (n - 1)

let rows (m : Tui_turbo.model) : string list = Curses.text (turbo.view m)
let has (m : Tui_turbo.model) (s : string) : bool = List.exists (fun row -> contains row s) (rows m)


let turbo_tests =
  [ Testo.create "Turbo: QUEENS.PAS in the blue window, the menu bar and the status line" (fun () ->
        let m = turbo.init in
        check "screen" "edit" (Tui_turbo.screen m);
        let r = rows m in
        Alcotest.(check bool) "the bar" true (contains (List.hd r) "File  Search  Run  Compile  Debug  Help");
        Alcotest.(check bool) "the title in the frame" true (contains (List.nth r 1) "╔" && contains (List.nth r 1) " QUEENS.PAS ");
        Alcotest.(check bool) "the status line" true (contains (List.nth r 23) "Ctrl+F9 Run"));
    Testo.create "Turbo: F9 compiles, the box says so" (fun () ->
        let m = tp_type turbo.init "\x1b[20~" in
        check "screen" "dialog" (Tui_turbo.screen m);
        Alcotest.(check bool) "done" true (has m "Done."));
    Testo.create "Turbo: Ctrl-F9 runs on the user screen; a key comes back" (fun () ->
        let m = tp_ticks (tp_type turbo.init "\x1b[20;5~") 1000 in
        check "screen" "user" (Tui_turbo.screen m);
        Alcotest.(check bool) "the queens" true (has m "92 solutions");
        check "back" "edit" (Tui_turbo.screen (tp_type m "x")));
    Testo.create "Turbo: an error in a red bar, the cursor on it" (fun () ->
        let m = tp_type turbo.init "x\x1b[20~" in
        Alcotest.(check (option string)) "error" (Some "Error: program expected, not xprogram.") (Tui_turbo.error m);
        Alcotest.(check (pair int int)) "cursor" (0, 0) (Tui_turbo.cursor m);
        Alcotest.(check (option string)) "a key clears it" None (Tui_turbo.error (tp_type m "\x1b[B")));
    Testo.create "Turbo: a run-time error brings the cursor to its line" (fun () ->
        (* File / New, a program typed, Enter keeping the indentation *)
        let m = tp_type turbo.init "\x1bfn" in
        let m = tp_type m "program T;\rvar a: array[1..3] of integer; i: integer;\rbegin\r  i := 4;\ra[i] := 0\rend." in
        check "autoindent" "  a[i] := 0" (List.nth (Tui_turbo.lines m) 4);
        let m = tp_ticks (tp_type m "\x1b[20;5~") 100 in
        check "screen" "user" (Tui_turbo.screen m);
        let m = tp_type m " " in
        Alcotest.(check (option string)) "error" (Some "Runtime error 201: Range check error.") (Tui_turbo.error m);
        Alcotest.(check int) "its line" 4 (fst (Tui_turbo.cursor m)));
    Testo.create "Turbo: menus by Alt and a letter, the Open dialog, F2" (fun () ->
        let m = tp_type turbo.init "\x1bc" in
        check "menu" "menu" (Tui_turbo.screen m);
        Alcotest.(check bool) "its items" true (has m "P-code");
        check "p: the listing" "p-code" (Tui_turbo.screen (tp_type m "p"));
        let m = tp_type turbo.init "\x1bOR\r" in
        Alcotest.(check bool) "FACT.PAS, the first" true (List.nth (Tui_turbo.lines m) 0 = "program Factorials;");
        let m = tp_type m "{ saved }\x1bOQ" in
        check "saved" "{ saved }program Factorials;" (List.hd (String.split_on_char '\n' (Option.get (Tui_turbo.file m "FACT.PAS"))))) ]

(* the debugger: keys as xterm sends them *)
let f7 = "\x1b[18~"
let f8 = "\x1b[19~"
let ctrl_f7 = "\x1b[18;5~"
let ctrl_f8 = "\x1b[19;5~"
let ctrl_f9 = "\x1b[20;5~"

(* the cursor to a line (from 1), by Search / Go to line number *)
let goto (m : Tui_turbo.model) (l : int) : Tui_turbo.model = tp_type m (Printf.sprintf "\x1bsg%d\r" l)

let line_with (m : Tui_turbo.model) (text : string) : int =
  let rec go i = function [] -> Alcotest.fail text | l :: rest -> if contains l text then i else go (i + 1) rest in
  go 1 (Tui_turbo.lines m)

let debugger_tests =
  [ Testo.create "Turbo's debugger: F7 starts at the main begin, the execution bar there" (fun () ->
        let m = tp_type turbo.init f7 in
        let main = line_with m "  for i := 1 to 8 do a[i] := true;" - 1 in
        Alcotest.(check (option int)) "the bar" (Some (main - 1)) (Tui_turbo.execution_line m);
        Alcotest.(check int) "the cursor on it" (main - 1) (fst (Tui_turbo.cursor m));
        check "the editor, no flash of the user screen" "edit" (Tui_turbo.screen m));
    Testo.create "Turbo's debugger: a breakpoint, Ctrl-F9 to it twice, a watch, the call stack" (fun () ->
        let m = turbo.init in
        let bp = line_with m "x[j] := i;" in
        let m = tp_type (goto m bp) ctrl_f8 in
        let m = tp_type m ctrl_f9 in
        let m = tp_ticks m 100 in
        let m = tp_ticks (tp_type m ctrl_f9) 100 in
        Alcotest.(check (option int)) "at the breakpoint" (Some (bp - 1)) (Tui_turbo.execution_line m);
        (* Ctrl-F7 offers the word under the cursor: x, from the line's start *)
        let m = tp_type m ("\x1b[C\x1b[C\x1b[C\x1b[C\x1b[C\x1b[C" ^ ctrl_f7 ^ "\r") in
        Alcotest.(check bool) "the watch" true (has m "x: (1,0,0,0,0,0,0,0)");
        let m = tp_type m "\x1bdc" in
        Alcotest.(check bool) "TRY(2) in the call stack" true (has m "TRY(2)" && has m "static link 0"));
    Testo.create "Turbo's debugger: F8 over a readln, the line typed on the user screen" (fun () ->
        let m = tp_type turbo.init "\x1bOR" in
        (* GUESS.PAS: the second file, after FACT.PAS *)
        let m = tp_type m "\x1b[B\r" in
        check "guess" "program Guess;" (List.hd (Tui_turbo.lines m));
        let m = goto m (line_with m "readln(guess)") in
        let m = tp_ticks (tp_type m "\x1bOS") 50 in
        let m = tp_type m f8 in
        check "reading: the user screen" "run" (Tui_turbo.screen m);
        let m = tp_type m "50\r" in
        Alcotest.(check (option int)) "the next line" (Some (line_with m "tries := tries + 1;" - 1)) (Tui_turbo.execution_line m));
    Testo.create "Turbo's debugger: Ctrl-C breaks a loop that never ends" (fun () ->
        let m = tp_type turbo.init "\x1bfn" in
        let m = tp_type m "program T;\rvar i: integer;\rbegin\r  while true do\ri := i + 1\rend." in
        let m = tp_ticks (tp_type m ctrl_f9) 5 in
        check "running" "run" (Tui_turbo.screen m);
        let m = tp_type m "\x03" in
        Alcotest.(check bool) "paused in the loop" true (match Tui_turbo.execution_line m with Some l -> l = 3 || l = 4 | None -> false)) ]

let tests = Testo.categorize "Editor" (gap_tests @ emacs_tests @ vi_tests @ turbo_tests @ debugger_tests)
