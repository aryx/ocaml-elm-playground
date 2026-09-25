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
  [ "1 REM GUESS THE NUMBER: THE FIRST PROGRAM OF MANY A BASIC COURSE,";
    "2 REM AS GUESS IN DAVID AHL, 101 BASIC COMPUTER GAMES (DEC, 1973).";
    "3 REM OUR OWN LISTING: THE SAME GAME AS TTY_GUESS, THE TESTS CHECK IT.";
    "10 PRINT \"GUESS THE NUMBER\"";
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
  [ "1 REM BAGELS, A GAME OF THE LAWRENCE HALL OF SCIENCE, BERKELEY,";
    "2 REM AS BAGELS IN DAVID AHL, BASIC COMPUTER GAMES (CREATIVE";
    "3 REM COMPUTING, 1978). OUR OWN LISTING OF IT.";
    "10 PRINT TAB(15); \"BAGELS\"";
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
  [ "1 REM THE MANDELBROT SET, 79 BY 23: BENOIT MANDELBROT, THE FRACTAL";
    "2 REM GEOMETRY OF NATURE (1982); THE LISTING EVERY MAGAZINE PRINTED";
    "3 REM AFTER A. K. DEWDNEY, COMPUTER RECREATIONS, SCIENTIFIC AMERICAN";
    "4 REM (AUGUST 1985).";
    "10 REM Z -> Z * Z + C, A CHARACTER FOR HOW SOON Z PASSES 2";
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
  [ "1 REM WACLAW SIERPINSKI'S TRIANGLE (1915), DRAWN BY THE ODD NUMBERS";
    "2 REM OF PASCAL'S TRIANGLE (1654): SEE MANDELBROT, THE FRACTAL";
    "3 REM GEOMETRY OF NATURE (1982).";
    "10 REM PASCAL'S TRIANGLE, ITS ODD NUMBERS AS STARS";
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
  [ "1 REM AFTER SINE WAVE, IN DAVID AHL, BASIC COMPUTER GAMES (1978),";
    "2 REM WHICH WAVED CREATIVE AND COMPUTING; OURS WAVES TINY AND BASIC.";
    "10 REM THE WORDS AT TAB(16 + 15 * SIN(T))";
    "20 B = 0";
    "30 FOR T = 0 TO 30 STEP .25";
    "40 IF B = 0 THEN PRINT TAB(INT(16 + 15 * SIN(T))); \"TINY\" : B = 1 : GOTO 60";
    "50 PRINT TAB(INT(16 + 15 * SIN(T))); \"BASIC\" : B = 0";
    "60 NEXT T" ]

(* the strategy is Nim's: leave one more than a multiple of 4 *)
let matches =
  [ "1 REM 23 MATCHES: TAKE 1, 2 OR 3; WHOEVER TAKES THE LAST ONE LOSES.";
    "2 REM AFTER BOB ALBRECHT (PEOPLE'S COMPUTER COMPANY), IN DAVID AHL,";
    "3 REM BASIC COMPUTER GAMES (1978). A NIM: WHOEVER LEAVES 4K+1 WINS.";
    "10 N = 23";
    "20 PRINT \"23 MATCHES. TAKE 1, 2 OR 3; THE LAST ONE LOSES.\"";
    "30 PRINT : PRINT \"THERE ARE \"; N; \" MATCHES.\"";
    "40 PRINT \"HOW MANY DO YOU TAKE\";";
    "50 INPUT T";
    "60 IF T < 1 OR T > 3 OR T > N THEN PRINT \"1, 2 OR 3, PLEASE.\" : GOTO 40";
    "70 N = N - T";
    "80 IF N = 0 THEN PRINT \"YOU TOOK THE LAST ONE. I WIN!\" : END";
    "90 REM LEAVE ONE MORE THAN A MULTIPLE OF 4, IF I CAN";
    "100 C = N - 1 - 4 * INT((N - 1) / 4) : IF C = 0 THEN C = 1";
    "110 PRINT \"I TAKE \"; C; \".\"";
    "120 N = N - C";
    "130 IF N = 0 THEN PRINT \"I TOOK THE LAST ONE. YOU WIN!\" : END";
    "140 GOTO 30" ]

(* a binary tree in three arrays: Q$(K) a question, or at a leaf an
   animal; Y(K) and N(K) where yes and no lead, 0 at a leaf; a wrong
   guess turns the leaf into a question and its two answers *)
let animal =
  [ "1 REM ANIMAL: THE COMPUTER LEARNS A TREE OF QUESTIONS AS IT LOSES.";
    "2 REM AFTER ARTHUR LUEHRMANN (DARTMOUTH), NATHAN TEICHHOLTZ AND";
    "3 REM STEVE NORTH, IN DAVID AHL, BASIC COMPUTER GAMES (1978).";
    "10 DIM Q$(100), Y(100), N(100)";
    "20 Q$(1) = \"DOES IT SWIM\" : Y(1) = 2 : N(1) = 3";
    "30 Q$(2) = \"FISH\" : Q$(3) = \"BIRD\" : M = 3";
    "40 PRINT : PRINT \"THINK OF AN ANIMAL. I WILL TRY TO GUESS IT.\"";
    "50 K = 1";
    "60 IF Y(K) = 0 THEN 110";
    "70 PRINT Q$(K);";
    "80 INPUT A$ : A$ = LEFT$(A$, 1)";
    "90 IF A$ = \"Y\" OR A$ = \"y\" THEN K = Y(K) : GOTO 60";
    "100 K = N(K) : GOTO 60";
    "110 PRINT \"IS IT A \"; Q$(K);";
    "120 INPUT A$ : A$ = LEFT$(A$, 1)";
    "130 IF A$ = \"Y\" OR A$ = \"y\" THEN PRINT \"WHY NOT TRY ANOTHER ANIMAL?\" : GOTO 40";
    "140 PRINT \"THE ANIMAL YOU WERE THINKING OF WAS A\";";
    "150 INPUT V$";
    "160 PRINT \"A QUESTION TO TELL A \"; V$; \" FROM A \"; Q$(K)";
    "170 INPUT W$";
    "180 PRINT \"FOR A \"; V$; \", THE ANSWER WOULD BE\";";
    "190 INPUT A$ : A$ = LEFT$(A$, 1)";
    "200 Q$(M + 1) = Q$(K) : Q$(M + 2) = V$ : Q$(K) = W$";
    "210 IF A$ = \"Y\" OR A$ = \"y\" THEN Y(K) = M + 2 : N(K) = M + 1 : GOTO 230";
    "220 Y(K) = M + 1 : N(K) = M + 2";
    "230 M = M + 2 : GOTO 40" ]

(* every 5 seconds: gravity adds 8 m/s, a unit of fuel burnt takes
   2.5 away; the height goes down by the mean speed times 5 *)
let lunar =
  [ "1 REM LUNAR: LAND ON THE MOON, A BURN EVERY 5 SECONDS.";
    "2 REM AFTER JIM STORER'S LUNAR (PDP-8, FOCAL, 1969), THE FIRST LUNAR";
    "3 REM LANDER, IN DAVID AHL, BASIC COMPUTER GAMES (1978). OUR PHYSICS,";
    "4 REM SIMPLER THAN HIS.";
    "10 H = 1000 : V = 40 : F = 60 : T = 0";
    "20 PRINT \"LUNAR: A BURN OF 0 TO 10 EVERY 5 SECONDS.\"";
    "30 PRINT \"LAND AT 5 M/S OR LESS.\" : PRINT";
    "40 PRINT \"SEC\"; TAB(6); \"HEIGHT\"; TAB(14); \"SPEED\"; TAB(22); \"FUEL\"";
    "50 PRINT T; TAB(6); INT(H); TAB(14); INT(V * 10) / 10; TAB(22); F";
    "60 IF F = 0 THEN B = 0 : GOTO 100";
    "70 PRINT \"BURN\";";
    "80 INPUT B";
    "90 IF B < 0 OR B > 10 THEN PRINT \"0 TO 10, PLEASE.\" : GOTO 70";
    "95 IF B > F THEN B = F";
    "100 F = F - B : W = V + 8 - 2.5 * B";
    "110 H = H - (V + W) / 2 * 5 : V = W : T = T + 5";
    "120 IF H > 0 THEN 50";
    "130 PRINT \"TOUCHDOWN AT \"; INT(V * 10) / 10; \" M/S.\"";
    "140 IF V <= 5 THEN PRINT \"A PERFECT LANDING!\" : END";
    "150 IF V <= 15 THEN PRINT \"A HARD LANDING. THE CRAFT IS DAMAGED.\" : END";
    "160 PRINT \"YOU DUG A CRATER \"; INT(V / 3); \" METERS DEEP.\"" ]

let files =
  [ { name = "GUESS"; dialect = Integer; lines = guess };
    { name = "BAGELS"; dialect = Applesoft; lines = bagels };
    { name = "MANDEL"; dialect = Applesoft; lines = mandel };
    { name = "SIERPINSKI"; dialect = Applesoft; lines = sierpinski };
    { name = "SINE"; dialect = Applesoft; lines = sine };
    { name = "MATCHES"; dialect = Integer; lines = matches };
    { name = "ANIMAL"; dialect = Applesoft; lines = animal };
    { name = "LUNAR"; dialect = Applesoft; lines = lunar } ]

(*****************************************************************************)
(* The catalog *)
(*****************************************************************************)

let catalog (files : file list) : string =
  let sectors (f : file) = (List.fold_left (fun n l -> n + String.length l + 1) 0 f.lines / 256) + 1 in
  "DISK VOLUME 254\n\n"
  ^ String.concat ""
      (List.map (fun f -> Printf.sprintf " %c %03d %s\n" (match f.dialect with Integer -> 'I' | Applesoft -> 'A') (sectors f) f.name) files)
