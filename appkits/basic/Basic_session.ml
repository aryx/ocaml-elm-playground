(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Teletype

(* See Basic_session.mli *)

let guess =
  [ "10 PRINT \"GUESS THE NUMBER\"";
    "20 PRINT";
    "30 PRINT \"WHAT LIMIT DO YOU WANT\";";
    "40 INPUT L";
    "50 IF L > 0 THEN 80";
    "60 PRINT \"A NUMBER, PLEASE.\"";
    "70 GOTO 30";
    "80 N = RND(L)";
    "90 PRINT";
    "100 PRINT \"I'M THINKING OF A NUMBER FROM 1 TO \"; L; \".\"";
    "110 T = 1";
    "120 PRINT \"YOUR GUESS\";";
    "130 INPUT G";
    "140 IF G > 0 THEN 170";
    "150 PRINT \"A NUMBER, PLEASE.\"";
    "160 GOTO 120";
    "170 IF G < N THEN 200";
    "180 IF G > N THEN 230";
    "190 GOTO 260";
    "200 PRINT \"TOO LOW.\"";
    "210 T = T + 1";
    "220 GOTO 120";
    "230 PRINT \"TOO HIGH.\"";
    "240 T = T + 1";
    "250 GOTO 120";
    "260 PRINT \"THAT'S IT! YOU GOT IT IN \"; T;";
    "270 IF T = 1 THEN PRINT \" TRY.\"";
    "280 IF T > 1 THEN PRINT \" TRIES.\"";
    "290 REM B: THE MOST GUESSES HALVING NEEDS";
    "300 B = 1";
    "310 M = L";
    "320 IF M <= 1 THEN 360";
    "330 M = M / 2";
    "340 B = B + 1";
    "350 GOTO 320";
    "360 IF T <= B THEN PRINT \"GOOD: HALVING WOULDN'T HAVE DONE BETTER.\"";
    "370 IF T > B THEN PRINT \"HALVING WHAT IS LEFT NEVER TAKES MORE THAN \"; B; \".\"";
    "380 PRINT";
    "390 PRINT \"PLAY AGAIN\";";
    "400 INPUT A$";
    "410 IF LEFT$(A$, 1) = \"Y\" OR LEFT$(A$, 1) = \"y\" THEN 80";
    "420 PRINT \"BYE!\"";
    "430 END" ]

let session ~(dialect : Basic_run.dialect) ~(program : Basic_run.program) (banner : string) : unit talk =
  let rec prompt (dialect : Basic_run.dialect) (program : Basic_run.program) : unit talk =
    (* the Apple II's prompts: > for Integer BASIC, ] for Applesoft *)
    let* line = ask (match dialect with Integer -> ">" | Applesoft -> "]") in
    if String.trim line = "" then prompt dialect program
    else
      match Basic_parse.parse_line line with
      | Error msg ->
          let* () = print (match dialect with Integer -> "*** SYNTAX ERR: " ^ msg ^ "\n" | Applesoft -> "?SYNTAX ERROR: " ^ msg ^ "\n") in
          prompt dialect program
      | Ok (Numbered (n, None)) -> prompt dialect (Basic_run.remove program n)
      | Ok (Numbered (n, Some stmts)) -> prompt dialect (Basic_run.add program n (Basic_run.text_of line) stmts)
      | Ok (Direct [ List ]) ->
          let* () = print (Basic_run.listing program) in
          prompt dialect program
      | Ok (Direct [ New ]) -> prompt dialect Basic_run.empty
      | Ok (Direct [ Bye ]) -> return ()
      | Ok (Direct [ Fp ]) -> prompt Applesoft program
      | Ok (Direct [ Int ]) -> prompt Integer program
      | Ok (Direct stmts) ->
          let child = match stmts with [ Run ] -> Basic_run.run dialect program | _ -> Basic_run.direct dialect program stmts in
          let* status = spawn child in
          let* () = if status = Interrupted then print "*** BREAK\n" else return () in
          prompt dialect program
  in
  let* () = print banner in
  prompt dialect program
