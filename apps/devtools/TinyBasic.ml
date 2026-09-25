(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tiny BASIC (Dennis Allison, 1975; Li-Chen Wang's
 * Palo Alto Tiny BASIC, 1976) on the screen of a 1977 home computer:
 * type a program a numbered line at a time, LIST it, RUN it.
 *
 *   type a line and enter: 10 PRINT "HI" (a line of the program; 10
 *   alone deletes it), PRINT 2 + 2 (done at once), LIST, RUN, NEW; FP
 *   for Applesoft (Microsoft's BASIC, floating point), INT back;
 *   Control-C stops a running program; flags: phosphor=white or amber
 *   (green, the Apple Monitor II's, by default)
 *
 * The Apple II (1977) booted into Integer BASIC, Steve Wozniak's own:
 * a ">" prompt on 40 columns of capitals, and numbers that were
 * integers of 16 bits, as Tiny BASIC's were -- both written for
 * machines whose programmers counted bytes. A year later came
 * Applesoft, Microsoft's BASIC in the Apple's ROM, with floating point,
 * and the DOS commands FP and INT to go from one to the other. This is
 * that screen, with both (Basic_parse.mli has their grammar,
 * Basic_run.mli their two arithmetics). The program Guess the Number
 * is typed in already: LIST shows it, RUN plays it, and it is the same
 * game as Tty_guess, which the tests check.
 *
 * The prompt is the whole programming environment of 1977
 * (Basic_session.mli): the editor, the calculator and the shell in
 * one. The interpreter is a Talk program (Basic_run.mli): INPUT a
 * question whose continuation is the rest of the program; each line a
 * step, so that 10 GOTO 10 keeps running a frame at a time, and
 * Control-C breaks it.
 *
 * What it uses: the Playground, its Teletype way (over libs/terminal's
 * Talk, Vt and Line_discipline), and libs/languages/basic. TinyTerminal's shell
 * runs the same BASIC as its command basic.
 *
 * Left undone, exercises: a BASIC listing of Hunt the Wumpus to play
 * against Tty_wumpus, as Guess is against Tty_guess (it needs a way to
 * draw the same random numbers: RND(20) in Integer BASIC); SAVE and LOAD
 * (a cassette's worth of Playground_platform.store); the Apple II's
 * low-resolution graphics, GR, COLOR= and PLOT, 40 by 40 blocks of 16
 * colours; the screen editor of Commodore's BASIC, where the cursor
 * goes up to a listed line to change it in place.
 *)
open Talk

let banner = "TINY BASIC, 1976, ON A 1977 SCREEN\nGUESS THE NUMBER IS TYPED IN:\nLIST, RUN, OR NEW TO START AFRESH.\nFP FOR APPLESOFT, INT TO COME BACK.\nCATALOG: THE DISK; RUN SIERPINSKI...\n\n"

let guess = match Basic_run.of_lines Basic_disk.guess with Ok p -> p | Error msg -> failwith msg

let phosphor (flags : Playground.flags) : Playground.color =
  match List.assoc_opt "phosphor" flags with
  | Some "white" -> Playground.rgb 215 220 235
  | Some "amber" -> Playground.rgb 255 176 0
  | _ -> Playground.rgb 90 235 120

(* the screen in a monitor's dark frame *)
let view (computer : Playground.computer) (m : machine) : Playground.shape list =
  let open Playground in
  let w, h = Teletype.size computer m in
  [ rectangle (rgb 40 38 34) computer.screen.width computer.screen.height;
    rectangle (rgb 20 20 18) (w +. 40.) (h +. 40.);
    group (Teletype.draw ~capitals:true ~phosphor:(phosphor computer.flags) computer m) ]

let app = Teletype.teletype ~rows:24 ~cols:40 ~view (Basic_session.session ~dialect:Integer ~program:guess banner)

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
