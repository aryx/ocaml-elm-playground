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
let output ?(seed = 1) ?(dialect = Basic_run.Integer) (lines : string list) (answers : string list) : string =
  Teletype.run ~seed (Basic_run.run dialect (program lines)) answers

let fp = output ~dialect:Basic_run.Applesoft
let check = Alcotest.(check string)

let tests =
  Testo.categorize "Basic"
    [
      (* parsing *)
      Testo.create "the worked example: IF X < 5 THEN PRINT \"small\"; X" (fun () ->
          Alcotest.(check bool) "parsed" true
            (parse_line "10 if x<5 then print \"small\";x"
            = Ok
                (Numbered
                   (10, Some [ If (Bin (Lt, Var "X", Num 5.)); Print [ (Expr (Str "small"), Semi); (Expr (Var "X"), Newline) ] ])
                ));
          Alcotest.(check bool) "no spaces at all" true
            (parse_line "IFX<5THEN10" = Ok (Direct [ If (Bin (Lt, Var "X", Num 5.)); Goto (Num 10.) ])));
      Testo.create "the cruncher: FORI=1TO9 reads, TOTAL = 5 doesn't" (fun () ->
          Alcotest.(check bool) "FORI=1TO9" true (parse_line "FORI=1TO9" = Ok (Direct [ For ("I", Num 1., Num 9., None) ]));
          Alcotest.(check bool) "TO TAL" true (Result.is_error (parse_line "TOTAL = 5")));
      Testo.create "precedence: -2^2, 1+2*3, NOT and AND" (fun () ->
          Alcotest.(check bool) "-2^2" true (parse_line "A = -2^2" = Ok (Direct [ Let (Scalar "A", Neg (Bin (Pow, Num 2., Num 2.))) ]));
          Alcotest.(check bool) "1+2*3" true
            (parse_line "A = 1 + 2 * 3" = Ok (Direct [ Let (Scalar "A", Bin (Add, Num 1., Bin (Mul, Num 2., Num 3.))) ]));
          check "values" "-4 7 0 1\n" (fp [ "10 PRINT -2^2; \" \"; 1+2*3; \" \"; NOT 5; \" \"; 1<2 AND 2<3" ] []));
      Testo.create "refused: with the reason" (fun () ->
          Alcotest.(check bool) "no THEN" true (parse_line "IF X < 5 PRINT X" = Error "THEN EXPECTED");
          Alcotest.(check bool) "no TO" true (parse_line "FOR I = 1" = Error "TO EXPECTED");
          Alcotest.(check bool) "nothing to go to" true (parse_line "GOTO" = Error "EXPRESSION EXPECTED"));
      (* Integer BASIC *)
      Testo.create "Integer: 16 bits, 32767 + 1 is -32768, division truncates" (fun () ->
          check "wrap" "-32768\n-3\n" (output [ "10 A = 32767"; "20 PRINT A + 1"; "30 PRINT -7 / 2" ] []));
      Testo.create "Integer: GOSUB and RETURN, a computed GOTO" (fun () ->
          check "out" "SUB\nBACK\nTWO\n"
            (output
               [ "10 GOSUB 100"; "20 PRINT \"BACK\""; "30 N = 2"; "40 GOTO 200 + N * 10"; "100 PRINT \"SUB\""; "110 RETURN";
                 "210 PRINT \"ONE\""; "215 END"; "220 PRINT \"TWO\"" ]
               []));
      Testo.create "Integer: PRINT's columns of 8, a line kept open for INPUT" (fun () ->
          check "commas" "1       22      333\n" (output [ "10 PRINT 1, 22, 333" ] []);
          check "prompt" "NAME? 7\n7\n" (output [ "10 PRINT \"NAME\";"; "20 INPUT N"; "30 PRINT N" ] [ "7" ]);
          check "reenter" "? X\n?REENTER\n? 3\n3\n" (output [ "10 INPUT N"; "20 PRINT N" ] [ "X"; "3" ]));
      Testo.create "Integer: errors stop the program, with their line" (fun () ->
          check "no line" "*** NO LINE 45 ERR IN 10\n" (output [ "10 GOTO 45"; "20 PRINT \"NEVER\"" ] []);
          check "zero" "*** DIVISION BY ZERO ERR IN 10\n" (output [ "10 PRINT 1 / 0" ] []);
          check "return" "*** RETURN WITHOUT GOSUB ERR IN 10\n" (output [ "10 RETURN" ] []));
      (* Applesoft *)
      Testo.create "Applesoft: floating point, printed its way" (fun () ->
          check "numbers" "3.5 .333333333 1024 -4 1E+10 -.5\n"
            (fp [ "10 PRINT 7/2; \" \"; 1/3; \" \"; 2^10; \" \"; INT(-3.5); \" \"; 1E10; \" \"; -1/2" ] []);
          check "columns of 16" "1               2\n" (fp [ "10 PRINT 1, 2" ] []);
          check "errors" "?NEXT WITHOUT FOR ERROR IN 10\n" (fp [ "10 NEXT I" ] []));
      Testo.create "Applesoft: RND(1) from 0 to 1, a die from it" (fun () ->
          let dice =
            fp ~seed:3 [ "10 FOR I = 1 TO 200"; "20 D = INT(RND(1) * 6) + 1"; "30 IF D < 1 OR D > 6 THEN PRINT \"BAD\""; "40 NEXT I"; "50 PRINT \"OK\"" ] []
          in
          check "within" "OK\n" dice);
      Testo.create "strings and their functions" (fun () ->
          check "out" "HELLO, WORLD 5 ELL LO A 65 12 3.5\n"
            (fp
               [ "10 A$ = \"HELLO\"";
                 "20 B$ = A$ + \", \" + \"WORLD\"";
                 "30 PRINT B$; \" \"; LEN(A$); \" \"; MID$(A$, 2, 3); \" \"; RIGHT$(A$, 2); \" \"; CHR$(65); \" \"; ASC(\"A\"); \" \"; STR$(12); \" \"; VAL(\"3.5X\")" ]
               []);
          check "compare" "YES\n" (fp [ "10 IF \"ABC\" < \"ABD\" THEN PRINT \"YES\"" ] []);
          check "mismatch" "?TYPE MISMATCH ERROR IN 10\n" (fp [ "10 A$ = 5" ] []));
      Testo.create "FOR/NEXT: STEP, nesting, NEXT J, I" (fun () ->
          check "step" "10 7 4 1 \n" (fp [ "10 FOR I = 10 TO 1 STEP -3"; "20 PRINT I; \" \";"; "30 NEXT"; "40 PRINT" ] []);
          check "nested" "11 12 21 22 \n" (fp [ "10 FOR I = 1 TO 2 : FOR J = 1 TO 2"; "20 PRINT I; J; \" \";"; "30 NEXT J, I"; "40 PRINT" ] []));
      Testo.create "arrays: DIM, 11 without it, a bad subscript" (fun () ->
          check "sum" "55\n" (fp [ "10 FOR I = 1 TO 10 : A(I) = I : NEXT"; "20 FOR I = 1 TO 10 : S = S + A(I) : NEXT"; "30 PRINT S" ] []);
          check "2D" "6\n" (fp [ "10 DIM B(2, 3)"; "20 B(2, 3) = 6"; "30 PRINT B(2, 3)" ] []);
          check "bad" "?BAD SUBSCRIPT ERROR IN 10\n" (fp [ "10 A(11) = 1" ] []));
      Testo.create "DATA, READ, RESTORE, out of data" (fun () ->
          check "out" "APPLE 3 APPLE\n?OUT OF DATA ERROR IN 40\n"
            (fp [ "10 DATA APPLE, 3"; "20 READ A$, N : RESTORE : READ B$"; "30 PRINT A$; \" \"; N; \" \"; B$"; "40 READ C$, D$, E$" ] []));
      Testo.create "DEF FN, ON GOTO, IF guarding the rest of its line" (fun () ->
          check "fn" "25\n" (fp [ "10 DEF FNS(X) = X * X"; "20 PRINT FNS(5)" ] []);
          check "on" "B\n" (fp [ "10 ON 2 GOTO 20, 30"; "20 PRINT \"A\" : END"; "30 PRINT \"B\"" ] []);
          check "if" "3\n" (fp [ "10 IF 0 THEN PRINT 1 : PRINT 2"; "20 PRINT 3" ] []));
      Testo.create "names count by two characters; INPUT's fields" (fun () ->
          check "SCALE is SC" "5\n" (fp [ "10 SCALE = 5 : PRINT SC" ] []);
          Alcotest.(check bool) "SCORE is SC OR E" true (Result.is_error (parse_line "SCORE = 5"));
          check "fields" "? 1,2\nBOB 1 2\n" (fp [ "10 INPUT A, B"; "20 PRINT \"BOB \"; A; \" \"; B" ] [ "1,2" ]);
          check "missing" "NAME? BOB\n?? 3\nBOB3\n" (fp [ "10 INPUT \"NAME\"; N$, K"; "20 PRINT N$; K" ] [ "BOB"; "3" ]);
          check "extra" "? 1,2\n?EXTRA IGNORED\n1\n" (fp [ "10 INPUT A"; "20 PRINT A" ] [ "1,2" ]));
      (* the session *)
      Testo.create "the session: lines typed, replaced, deleted, LIST, RUN, FP, BYE" (fun () ->
          let s = Basic_session.session ~dialect:Integer ~program:Basic_run.empty "HI\n" in
          let out =
            Teletype.run s
              [ "20 print \"B\""; "10 PRINT \"A\""; "20 PRINT \"C\""; "30 X"; "30"; "LIST"; "RUN"; "PRINT 7/2"; "FP"; "PRINT 7/2"; "BYE" ]
          in
          check "transcript"
            ("HI\n>20 print \"B\"\n>10 PRINT \"A\"\n>20 PRINT \"C\"\n>30 X\n*** SYNTAX ERR: = EXPECTED\n>30\n"
           ^ ">LIST\n10 PRINT \"A\"\n20 PRINT \"C\"\n>RUN\nA\nC\n>PRINT 7/2\n3\n>FP\n]PRINT 7/2\n3.5\n]BYE\n")
            out);
      Testo.create "10 GOTO 10 runs a frame at a time, Control-C breaks it" (fun () ->
          let s = Basic_session.session ~dialect:Integer ~program:(program [ "10 GOTO 10" ]) "" in
          let m = Teletype.start ~seed:1 ~rows:4 ~cols:20 s in
          let m = Teletype.input m "RUN\r" in
          Alcotest.(check bool) "running, not reading" false (Teletype.reading m);
          let m = Teletype.tick m 0.02 in
          let m = Teletype.input m "\x03" in
          Alcotest.(check (list string)) "broken" [ ">RUN"; "^C"; "*** BREAK"; ">" ] (Vt.text (Teletype.screen m)));
      Testo.create "differential: the Guess listing plays Tty_guess's games" (fun () ->
          (* the same answers, the same seed: the BASIC program prints
             what the OCaml one does, games again and all *)
          let upto n = List.init n (fun i -> string_of_int (i + 1)) in
          List.iter
            (fun (seed, answers) ->
              let basic = Teletype.run ~seed (Basic_run.run Integer (program Basic_session.guess)) answers in
              let ocaml = Teletype.run ~seed Tty_guess.program answers in
              check (Printf.sprintf "seed %d" seed) ocaml basic)
            (* each game ends with every number up to the limit: a win *)
            [ (1, [ "100"; "50"; "25"; "75" ] @ upto 100 @ [ "Y"; "60"; "30" ] @ upto 100 @ [ "N" ]);
              (7, [ "0"; "10"; "-3"; "5" ] @ upto 10 @ [ "no" ]);
              (42, [ "1"; "1"; "" ]) ]);
    ]
