(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Basic_disk.mli *)

type file = { name : string; dialect : Basic_run.dialect; lines : string list }

(*****************************************************************************)
(* The programs *)
(*****************************************************************************)

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

let bagels =
  [ "10 PRINT TAB(15); \"BAGELS\"";
    "20 PRINT \"I THINK OF 3 DIFFERENT DIGITS.\"";
    "30 PRINT \"PICO: A DIGIT RIGHT, WRONG PLACE.\"";
    "40 PRINT \"FERMI: A DIGIT RIGHT, RIGHT PLACE.\"";
    "50 PRINT \"BAGELS: NO DIGIT RIGHT.\"";
    "60 DIM A(3), B(3)";
    "100 A(1) = INT(10 * RND(1))";
    "110 A(2) = INT(10 * RND(1)) : IF A(2) = A(1) THEN 110";
    "120 A(3) = INT(10 * RND(1)) : IF A(3) = A(1) OR A(3) = A(2) THEN 120";
    "130 PRINT : PRINT \"O.K. I HAVE A NUMBER IN MIND.\"";
    "140 FOR G = 1 TO 20";
    "150 PRINT \"GUESS #\"; G;";
    "160 INPUT A$";
    "170 IF LEN(A$) <> 3 THEN PRINT \"THREE DIGITS, PLEASE.\" : GOTO 150";
    "180 FOR I = 1 TO 3 : B(I) = VAL(MID$(A$, I, 1)) : NEXT I";
    "190 P = 0 : F = 0";
    "200 FOR I = 1 TO 3 : FOR J = 1 TO 3";
    "210 IF A(I) <> B(J) THEN 240";
    "220 IF I = J THEN F = F + 1 : GOTO 240";
    "230 P = P + 1";
    "240 NEXT J, I";
    "250 IF F = 3 THEN PRINT \"YOU GOT IT!!!\" : GOTO 320";
    "260 IF F + P = 0 THEN PRINT \"BAGELS\" : GOTO 300";
    "270 IF P > 0 THEN FOR I = 1 TO P : PRINT \"PICO \"; : NEXT I";
    "280 IF F > 0 THEN FOR I = 1 TO F : PRINT \"FERMI \"; : NEXT I";
    "290 PRINT";
    "300 NEXT G";
    "310 PRINT \"THAT'S TWENTY. MY NUMBER WAS \"; A(1); A(2); A(3); \".\"";
    "320 PRINT : PRINT \"PLAY AGAIN (YES OR NO)\";";
    "330 INPUT A$";
    "340 IF LEFT$(A$, 1) = \"Y\" OR LEFT$(A$, 1) = \"y\" THEN 100";
    "350 PRINT \"HOPE YOU HAD FUN. BYE.\"";
    "360 END" ]

(* how many times Z -> Z^2 + C stays within 2 of 0, a character for
   it; the points that never leave, the set itself, "@" *)
let mandel =
  [ "10 REM THE MANDELBROT SET, 79 BY 23";
    "20 C$ = \" .:-=+*#%\"";
    "30 FOR Y = -11 TO 11";
    "40 FOR X = -39 TO 39";
    "50 CA = X * .04 - .6 : CB = Y * .1";
    "60 A = CA : B = CB";
    "70 FOR I = 0 TO 26";
    "80 T = A * A - B * B + CA : B = 2 * A * B + CB : A = T";
    "90 IF A * A + B * B > 4 THEN PRINT MID$(C$, INT(I / 3) + 1, 1); : GOTO 120";
    "100 NEXT I";
    "110 PRINT \"@\";";
    "120 NEXT X";
    "130 PRINT";
    "140 NEXT Y" ]

(* each row of Pascal's triangle from the one above, kept modulo 2 *)
let sierpinski =
  [ "10 REM PASCAL'S TRIANGLE, ITS ODD NUMBERS AS STARS";
    "20 DIM R(33) : R(1) = 1";
    "30 FOR N = 1 TO 16";
    "40 PRINT TAB(18 - N);";
    "50 FOR K = 1 TO N : IF R(K) = 1 THEN PRINT \"* \"; : GOTO 70";
    "60 PRINT \"  \";";
    "70 NEXT K";
    "80 PRINT";
    "90 FOR K = N + 1 TO 2 STEP -1 : R(K) = R(K) + R(K - 1) : IF R(K) = 2 THEN R(K) = 0";
    "100 NEXT K";
    "110 NEXT N" ]

let sine =
  [ "10 REM A SINE WAVE, AFTER AHL'S";
    "20 B = 0";
    "30 FOR T = 0 TO 30 STEP .25";
    "40 IF B = 0 THEN PRINT TAB(INT(16 + 15 * SIN(T))); \"TINY\" : B = 1 : GOTO 60";
    "50 PRINT TAB(INT(16 + 15 * SIN(T))); \"BASIC\" : B = 0";
    "60 NEXT T" ]

let files =
  [ { name = "GUESS"; dialect = Integer; lines = guess };
    { name = "BAGELS"; dialect = Applesoft; lines = bagels };
    { name = "MANDEL"; dialect = Applesoft; lines = mandel };
    { name = "SIERPINSKI"; dialect = Applesoft; lines = sierpinski };
    { name = "SINE"; dialect = Applesoft; lines = sine } ]

(*****************************************************************************)
(* The catalog *)
(*****************************************************************************)

let catalog (files : file list) : string =
  let sectors (f : file) = (List.fold_left (fun n l -> n + String.length l + 1) 0 f.lines / 256) + 1 in
  "DISK VOLUME 254\n\n"
  ^ String.concat ""
      (List.map (fun f -> Printf.sprintf " %c %03d %s\n" (match f.dialect with Integer -> 'I' | Applesoft -> 'A') (sectors f) f.name) files)
