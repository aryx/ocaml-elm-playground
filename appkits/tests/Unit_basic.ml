(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Basic_parse

(* See Unit_basic.mli *)

let program (lines : string list) : Basic_run.program =
  match Basic_run.of_lines lines with Ok p -> p | Error msg -> Alcotest.fail msg

(* what a program prints, run on these answers *)
let output ?(seed = 1) (lines : string list) (answers : string list) : string =
  Teletype.run ~seed (Basic_run.run (program lines)) answers

let tests =
  Testo.categorize "Basic"
    [
      Testo.create "the worked example: IF X < 5 THEN PRINT \"small\"; X" (fun () ->
          Alcotest.(check bool) "parsed" true
            (parse_line "10 if x<5 then print \"small\";x"
            = Ok (Numbered (10, Some (If (Var 'X', Lt, Num 5, Print [ (Str "small", Semi); (Expr (Var 'X'), Newline) ])))));
          Alcotest.(check bool) "no spaces at all" true
            (parse_line "IFX<5THEN10" = Ok (Direct (If (Var 'X', Lt, Num 5, Goto (Num 10))))));
      Testo.create "precedence, the implicit LET, capitals outside strings" (fun () ->
          Alcotest.(check bool) "1+2*3" true
            (parse_line "a = -1 + 2 * 3" = Ok (Direct (Let ('A', Bin (Add, Neg (Num 1), Bin (Mul, Num 2, Num 3))))));
          Alcotest.(check string) "capitals" "PRINT \"Hi\";X" (capitals "print \"Hi\";x"));
      Testo.create "refused: with the reason" (fun () ->
          Alcotest.(check bool) "no THEN" true (parse_line "IF X < 5 PRINT X" = Error "THEN EXPECTED");
          Alcotest.(check bool) "too big" true (parse_line "PRINT 40000" = Error "NUMBER TOO BIG");
          Alcotest.(check bool) "junk after" true (parse_line "PRINT 1 2" = Error "SYNTAX");
          Alcotest.(check bool) "unterminated" true (parse_line "PRINT \"AB" = Error "UNTERMINATED STRING"));
      Testo.create "16 bits: 32767 + 1 is -32768; division truncates" (fun () ->
          Alcotest.(check string) "wrap" "-32768\n-3\n" (output [ "10 A = 32767"; "20 PRINT A + 1"; "30 PRINT -7 / 2" ] []));
      Testo.create "GOSUB and RETURN, a computed GOTO" (fun () ->
          Alcotest.(check string) "out" "SUB\nBACK\nTWO\n"
            (output
               [ "10 GOSUB 100"; "20 PRINT \"BACK\""; "30 N = 2"; "40 GOTO 200 + N * 10"; "100 PRINT \"SUB\""; "110 RETURN";
                 "210 PRINT \"ONE\""; "215 END"; "220 PRINT \"TWO\"" ]
               []));
      Testo.create "PRINT's columns of 8, and a line kept open for INPUT" (fun () ->
          Alcotest.(check string) "commas" "1       22      333\n" (output [ "10 PRINT 1, 22, 333" ] []);
          Alcotest.(check string) "prompt" "NAME? 7\n7\n" (output [ "10 PRINT \"NAME\";"; "20 INPUT N"; "30 PRINT N" ] [ "7" ]);
          Alcotest.(check string) "reenter" "? X\n?REENTER\n? 3\n3\n" (output [ "10 INPUT N"; "20 PRINT N" ] [ "X"; "3" ]));
      Testo.create "errors stop the program, with their line" (fun () ->
          Alcotest.(check string) "no line" "*** NO LINE 45 ERR IN 10\n" (output [ "10 GOTO 45"; "20 PRINT \"NEVER\"" ] []);
          Alcotest.(check string) "zero" "*** DIVISION BY ZERO ERR IN 10\n" (output [ "10 PRINT 1 / 0" ] []);
          Alcotest.(check string) "return" "*** RETURN WITHOUT GOSUB ERR IN 10\n" (output [ "10 RETURN" ] []));
      Testo.create "the session: lines typed, replaced, deleted, LIST, RUN, BYE" (fun () ->
          let s = Basic_session.session ~program:Basic_run.empty "HI\n" in
          let out = Teletype.run s [ "20 print \"B\""; "10 PRINT \"A\""; "20 PRINT \"C\""; "30 X"; "30"; "LIST"; "RUN"; "PRINT 2+2"; "BYE" ] in
          Alcotest.(check string) "transcript"
            ("HI\n>20 print \"B\"\n>10 PRINT \"A\"\n>20 PRINT \"C\"\n>30 X\n*** SYNTAX ERR: = EXPECTED\n>30\n"
           ^ ">LIST\n10 PRINT \"A\"\n20 PRINT \"C\"\n>RUN\nA\nC\n>PRINT 2+2\n4\n>BYE\n")
            out);
      Testo.create "10 GOTO 10 runs a frame at a time, Control-C breaks it" (fun () ->
          let s = Basic_session.session ~program:(program [ "10 GOTO 10" ]) "" in
          let m = Teletype.start ~seed:1 ~rows:4 ~cols:20 s in
          let m = Teletype.input m "RUN\r" in
          Alcotest.(check bool) "running, not reading" false (Teletype.reading m);
          let m = Teletype.tick m 0.02 in
          let m = Teletype.input m "\x03" in
          Alcotest.(check (list string)) "broken" [ ">RUN"; "^C"; "*** BREAK"; ">" ] (Vt.text (Teletype.screen m)));
      Testo.create "differential: the Guess listing plays Tty_guess's game" (fun () ->
          (* the same answers, the same seed: the BASIC program prints what
             the OCaml one does, up to its PLAY AGAIN (Tiny BASIC has no
             strings to read a yes in) *)
          List.iter
            (fun (seed, answers) ->
              let basic = Teletype.run ~seed (Basic_run.run (program Basic_session.guess)) answers in
              let ocaml = Teletype.run ~seed Tty_guess.program answers in
              (* the answers left over go to the OCaml game's PLAY AGAIN *)
              let played = String.sub ocaml 0 (min (String.length ocaml) (String.length basic + 13)) in
              Alcotest.(check string) (Printf.sprintf "seed %d" seed) played (basic ^ "\nPLAY AGAIN? "))
            (* each list ends with every number up to the limit: a win *)
            (let upto n = List.init n (fun i -> string_of_int (i + 1)) in
             [ (1, [ "100"; "50"; "25"; "75" ] @ upto 100); (7, [ "0"; "10"; "-3"; "5" ] @ upto 10); (42, [ "1"; "1" ]) ]));
    ]
