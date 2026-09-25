(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of a terminal and its shell, after the DEC VT100
 * (1978) and the Unix shell (Ken Thompson's, 1971; Stephen Bourne's,
 * 1979): a screen of 80 by 24 characters, and a prompt that runs
 * programs.
 *
 *   type a command and enter: help, ls, hangman, guess, wumpus, basic,
 *   colors, clear, echo, exit; Control-C stops a program, Control-U erases the line;
 *   flags: phosphor=green or amber (white, the VT100's, by default),
 *   baud=n (300 or 1200: a modem's speed, the echo included)
 *
 * A terminal is two machines people see as one. The VT100 is a
 * screen and a keyboard at the end of a wire: it shows the bytes it is
 * sent, escape sequences included (Vt.mli), and sends the keys typed.
 * The shell is a program on the computer at the other end, reading
 * lines and running programs -- the lines edited on the way by the
 * tty (Line_discipline.mli), the part between the two that neither
 * owns. The three are three modules here, and this file only puts them
 * together: the screen drawn in its case, the shell written as a
 * conversation (Teletype.mli), its commands in a table.
 *
 * The shell is the lesson in miniature: read a line, cut it into words,
 * the first word a program's name, run it, wait for it, read again.
 * Running is [spawn], Unix's fork, exec and wait in one, and waiting
 * is what makes Control-C work as on Unix: it interrupts the program
 * running, and the shell, waiting for it, prints its prompt again.
 * The programs are appkits/teletype's, the same values that run alone
 * (examples/TeletypeHangman.ml).
 *
 * What it uses: the Playground, its Teletype way (over libs/terminal's
 * Vt and Line_discipline), appkits/teletype's programs, and
 * appkits/basic's BASIC. Not gui/: a terminal's only widget is the
 * screen.
 *
 * Left undone, exercises: pipes and $VARIABLES (the Bourne shell's
 * language, of which this has the loop only); the history (up arrow:
 * the shell in raw mode, as bash's readline is); a real shell natively,
 * through a pty (plan_terminal.md, section 4); the VT100's SET-UP
 * screen, its 132 columns, its double-height letters.
 *)
open Teletype

(*****************************************************************************)
(* The shell *)
(*****************************************************************************)

(* the programs, each run by [spawn] *)
let programs : (string * string * unit talk) list =
  [ ("hangman", "guess the word before the man is hanged", Tty_hangman.program);
    ("guess", "guess the number, too low or too high", Tty_guess.program);
    ("wumpus", "hunt the Wumpus in a cave of 20 rooms", Tty_wumpus.program);
    ("basic", "Applesoft BASIC, a numbered line at a time (BYE: back here)",
     Basic_session.session ~dialect:Applesoft ~program:Basic_run.empty "APPLESOFT BASIC\n") ]

(* the escape sequences at work: SGR's colours, bold and reverse *)
let colors : string =
  let swatch n name = Printf.sprintf "\x1b[3%dm%-8s\x1b[0m" n name in
  String.concat "" (List.mapi swatch [ "black"; "red"; "green"; "yellow"; "blue"; "magenta"; "cyan"; "white" ])
  ^ "\n\x1b[1mbold\x1b[0m \x1b[7mreverse\x1b[0m \x1b[1;33;44m yellow on blue \x1b[0m\n"

let help : string =
  String.concat ""
    ([ "Commands:\n";
       "  help      this list\n";
       "  ls        the programs\n";
       "  clear     clear the screen (ESC [ 2 J)\n";
       "  echo      print its words\n";
       "  colors    the escape sequences' colours\n";
       "  exit      log out\n";
       "Programs (Control-C stops one):\n" ]
    @ List.map (fun (name, what, _) -> Printf.sprintf "  %-9s %s\n" name what) programs)

(* a line cut into its words, at the spaces *)
let split_words (line : string) : string list = String.split_on_char ' ' line |> List.filter (( <> ) "")

let rec shell () : unit talk =
  let* line = ask "$ " in
  match split_words line with
  | [] -> shell ()
  | [ "exit" ] -> print "logout\n"
  | cmd :: args ->
      let* () =
        match cmd with
        | "help" -> print help
        | "ls" -> print (String.concat "  " (List.map (fun (name, _, _) -> name) programs) ^ "\n")
        | "clear" -> print "\x1b[2J\x1b[H"
        | "echo" -> print (String.concat " " args ^ "\n")
        | "colors" -> print colors
        | _ -> (
            match List.find_opt (fun (name, _, _) -> name = cmd) programs with
            | Some (_, _, program) ->
                let* _status = spawn program in
                return ()
            | None -> print ("tsh: " ^ cmd ^ ": command not found\n"))
      in
      shell ()

let session : unit talk =
  let* () = print "TinyTerminal, a VT100 (DEC, 1978): 80 columns, 24 lines\n" in
  let* () = print "tsh, a shell: type help\n\n" in
  shell ()

(*****************************************************************************)
(* The VT100 *)
(*****************************************************************************)

(* the VT100's phosphor was white (P4); green (P1) and amber (P3) were
 * other terminals', and easier on the eyes *)
let phosphor (flags : Playground.flags) : Playground.color =
  match List.assoc_opt "phosphor" flags with
  | Some "green" -> Playground.rgb 120 230 120
  | Some "amber" -> Playground.rgb 255 176 0
  | _ -> Playground.rgb 215 220 235

let case = Playground.rgb 205 198 180
let dark = Playground.rgb 60 58 52
let desk = Playground.rgb 90 80 70

(* the screen in its case, and under it the keyboard's lights: ON LINE,
 * and KBD LOCKED while the computer is sending (the keyboard waits) *)
let view (computer : Playground.computer) (m : machine) : Playground.shape list =
  let open Playground in
  let screen = computer.screen in
  let k = 0.9 in
  let w, h = size computer m in
  let w, h = (w *. k, h *. k) in
  let y = 30. in
  let bottom = y -. (h /. 2.) -. 60. in
  let light on label x =
    group [ circle (if on then rgb 255 60 40 else rgb 90 30 25) 6.; words dark label |> move_x (12. +. (4.5 *. float_of_int (String.length label))) ]
    |> move x bottom
  in
  [ rectangle desk screen.width screen.height;
    rectangle case (w +. 80.) (h +. 180.) |> move_y (y -. 30.);
    rectangle dark (w +. 30.) (h +. 30.) |> move_y y;
    group (draw ~phosphor:(phosphor computer.flags) computer m) |> scale k |> move_y y;
    words dark "VT100" |> scale 1.4 |> move ((w /. 2.) -. 30.) bottom;
    light true "ON LINE" (-.(w /. 2.) +. 10.);
    light (not (reading m || finished m)) "KBD LOCKED" (-.(w /. 2.) +. 120.) ]

let app = teletype ~view session

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
