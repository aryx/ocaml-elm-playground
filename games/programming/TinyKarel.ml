(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Karel the Robot (Richard E. Pattis, Stanford, 1981), as it was
 * taught: you type a program in Pattis's language, press Run, and
 * watch the robot do what you said, which is not always what you
 * meant. A level is won when the world ends up as its task says.
 *
 *   the menu   the level (each keeps its own program)
 *   Run        from the level's start, at the slider's speed
 *   Step       one of the five primitives; Reset: back to the start
 *   Solution   one way to do it, in the editor (spoils the level)
 *
 * The robot, its world, and what running a program means are the
 * playground's Karel (Karel.mli, which explains Karel); this game adds
 * Pattis's language, parsed into Karel's commands, and the levels,
 * which are his exercises and Stanford's:
 *
 *   BEGINNING-OF-PROGRAM
 *     DEFINE-NEW-INSTRUCTION turnright AS
 *     BEGIN
 *       turnleft; turnleft; turnleft
 *     END;
 *     BEGINNING-OF-EXECUTION
 *       ITERATE 3 TIMES move;
 *       turnright;
 *       WHILE front-is-clear DO move;
 *       IF next-to-a-beeper THEN pickbeeper ELSE turnleft;
 *       turnoff
 *     END-OF-EXECUTION
 *   END-OF-PROGRAM
 *
 * The statements: move, turnleft, pickbeeper, putbeeper, turnoff, an
 * instruction you defined, BEGIN ... END, ITERATE n TIMES s, IF c THEN
 * s [ELSE s], WHILE c DO s. The eighteen conditions:
 * front-is-clear, left-is-clear, right-is-clear, and -blocked;
 * next-to-a-beeper, not-next-to-a-beeper; facing-north (east, south,
 * west), not-facing-north ...; any-beepers-in-beeper-bag,
 * no-beepers-in-beeper-bag. Comments in braces, as in Pascal, whose
 * syntax Pattis borrowed. Keywords in capitals, instructions in small
 * letters, as the book printed them, but either case is accepted; and
 * the semicolons are forgiven where Pascal would be strict, since they
 * are the first thing a beginner gets wrong and the least interesting.
 *
 * The parser is a recursive descent, a function per rule of the
 * grammar ([statement] calls itself for the body of an ITERATE, a
 * WHILE, an IF), after a first pass that collects the names of the new
 * instructions, so that one may be used before its definition, or
 * inside it. A mistake stops it, with its line, shown under the
 * editor; nothing runs until there is none (TinyCoreWar's way).
 *
 * The levels teach, in order: new words (the newspaper, where
 * turnright and turnaround pay for themselves), a word used twice
 * (the harvest, whose program is a row harvested, and a turn), a loop
 * of a word (the stairs), and the first program that works on a world
 * it has never seen (the maze, by the right-hand rule: keep your right
 * hand on the wall, and you get out of any maze whose walls hold
 * together -- the tests run it on a second maze too).
 *
 * What it uses: Karel (the world, the stepping, the drawing), Gui (the
 * editor -- gui/Text_edit -- the menu, the buttons, the slider), as
 * TinyCoreWar does.
 *
 * Exercises: Pattis's other exercises (a beeper put on every corner of
 * a room; the robot that finds its way home by counting beepers);
 * a beeper count per corner shown while editing a world, and the world
 * editor itself; Lightbot's version of the same idea, the program as
 * tiles instead of text.
 *)
open Playground

(*****************************************************************************)
(* Pattis's language *)
(*****************************************************************************)

exception Wrong of int * string

(* the words, the semicolons, and their lines; comments in braces
 * dropped *)
let tokens (text : string) : (string * int) list =
  let out = ref [] and word = Buffer.create 16 and line = ref 1 and comment = ref false in
  let flush () =
    if Buffer.length word > 0 then out := (Buffer.contents word, !line) :: !out;
    Buffer.clear word
  in
  String.iter
    (fun c ->
      if !comment then (if c = '}' then comment := false else if c = '\n' then incr line)
      else
        match c with
        | '{' -> flush (); comment := true
        | ';' -> flush (); out := (";", !line) :: !out
        | '\n' -> flush (); incr line
        | ' ' | '\t' | '\r' -> flush ()
        | c -> Buffer.add_char word (Char.lowercase_ascii c))
    text;
  flush ();
  List.rev !out

let primitives =
  [ ("move", Karel.move); ("turnleft", Karel.turn_left); ("pickbeeper", Karel.pick_beeper);
    ("putbeeper", Karel.put_beeper); ("turnoff", Karel.turn_off) ]

let conditions : (string * Karel.condition) list =
  let open Karel in
  let named name c = [ (name, c) ] in
  List.concat
    [ named "front-is-clear" front_is_clear; named "front-is-blocked" (not_ front_is_clear);
      named "left-is-clear" left_is_clear; named "left-is-blocked" (not_ left_is_clear);
      named "right-is-clear" right_is_clear; named "right-is-blocked" (not_ right_is_clear);
      named "next-to-a-beeper" next_to_a_beeper; named "not-next-to-a-beeper" (not_ next_to_a_beeper);
      named "any-beepers-in-beeper-bag" any_beepers_in_beeper_bag;
      named "no-beepers-in-beeper-bag" (not_ any_beepers_in_beeper_bag) ]
  @ List.concat_map
      (fun (name, d) -> [ ("facing-" ^ name, Karel.facing d); ("not-facing-" ^ name, Karel.not_ (Karel.facing d)) ])
      [ ("north", Karel.North); ("east", Karel.East); ("south", Karel.South); ("west", Karel.West) ]

type program = { definitions : (string * Karel.command list) list; main : Karel.command list }

(* [parse text]: the program, or the first mistake and its line *)
let parse (text : string) : (program, int * string) result =
  let toks = Array.of_list (tokens text) in
  let pos = ref 0 in
  let last_line = if Array.length toks = 0 then 1 else snd toks.(Array.length toks - 1) in
  let peek () = if !pos < Array.length toks then Some (fst toks.(!pos)) else None in
  let line () = if !pos < Array.length toks then snd toks.(!pos) else last_line in
  let next () =
    match peek () with
    | Some w -> incr pos; w
    | None -> raise (Wrong (last_line, "the program ends too soon"))
  in
  let expect w =
    let l = line () in
    let got = next () in
    if got <> w then raise (Wrong (l, Printf.sprintf "%s expected, not %s" (String.uppercase_ascii w) got))
  in
  let skip_semicolons () = while peek () = Some ";" do incr pos done in
  (* the new instructions' names, wherever they are defined *)
  let defined =
    List.filter_map Fun.id
      (List.mapi
         (fun i (w, _) -> if w = "define-new-instruction" && i + 1 < Array.length toks then Some (fst toks.(i + 1)) else None)
         (Array.to_list toks))
  in
  let rec statement () : Karel.command =
    let l = line () in
    match next () with
    | "begin" -> Karel.block (statements "end")
    | "iterate" -> (
        let n = next () in
        expect "times";
        match int_of_string_opt n with
        | Some n when n >= 0 -> Karel.iterate n [ statement () ]
        | _ -> raise (Wrong (l, "ITERATE wants a number, not " ^ n)))
    | "if" ->
        let c = condition () in
        expect "then";
        let yes = statement () in
        if peek () = Some "else" then (incr pos; Karel.if_ ~else_:[ statement () ] c [ yes ]) else Karel.if_ c [ yes ]
    | "while" ->
        let c = condition () in
        expect "do";
        Karel.while_ c [ statement () ]
    | w -> (
        match List.assoc_opt w primitives with
        | Some p -> p
        | None -> if List.mem w defined then Karel.call w else raise (Wrong (l, "no instruction " ^ w)))
  and condition () : Karel.condition =
    let l = line () in
    let w = next () in
    match List.assoc_opt w conditions with Some c -> c | None -> raise (Wrong (l, "no condition " ^ w))
  (* statements up to the word [stop], taken too *)
  and statements (stop : string) : Karel.command list =
    skip_semicolons ();
    if peek () = Some stop then (incr pos; [])
    else
      let s = statement () in
      s :: statements stop
  in
  try
    expect "beginning-of-program";
    let rec definitions () =
      skip_semicolons ();
      if peek () = Some "define-new-instruction" then (
        incr pos;
        let name = next () in
        if List.mem_assoc name primitives then raise (Wrong (line (), name ^ " is already an instruction"));
        expect "as";
        let body = statement () in
        (name, [ body ]) :: definitions ())
      else []
    in
    let definitions = definitions () in
    expect "beginning-of-execution";
    let main = statements "end-of-execution" in
    skip_semicolons ();
    expect "end-of-program";
    if peek () <> None then raise (Wrong (line (), "something after END-OF-PROGRAM"));
    Ok { definitions; main }
  with Wrong (l, msg) -> Error (l, msg)

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

type level = {
  name : string;
  task : string list; (* what to do, a line each *)
  world : string list; (* Karel.world's strings *)
  goal : Karel.world -> bool;
  solution : string;
}

let skeleton =
  "BEGINNING-OF-PROGRAM\n\
  \  BEGINNING-OF-EXECUTION\n\
  \    move;\n\
  \    turnoff\n\
  \  END-OF-EXECUTION\n\
   END-OF-PROGRAM\n"

let none_left (w : Karel.world) = Karel.beepers_left w = 0

let levels =
  [ { name = "The newspaper";
      task = [ "Fetch the newspaper from the porch,"; "and come back to where you started." ];
      world =
        [ ". . . . . .";
          "  - - -    ";
          ".|> . .|. .";
          "           ";
          ".|. . . . 1";
          "  - - -    ";
          ". . . . . ." ];
      goal = (fun w -> none_left w && (let a, s, _ = Karel.karel w in (a, s) = (2, 3)));
      solution =
        "BEGINNING-OF-PROGRAM\n\
        \  DEFINE-NEW-INSTRUCTION turnright AS\n\
        \  BEGIN\n\
        \    turnleft; turnleft; turnleft\n\
        \  END;\n\
        \  DEFINE-NEW-INSTRUCTION turnaround AS\n\
        \  BEGIN\n\
        \    turnleft; turnleft\n\
        \  END;\n\
        \  BEGINNING-OF-EXECUTION\n\
        \    { out of the door, to the porch }\n\
        \    move; move; turnright; move; turnleft;\n\
        \    move; move; pickbeeper;\n\
        \    { and back }\n\
        \    turnaround; move; move;\n\
        \    turnright; move; turnleft; move; move;\n\
        \    turnaround; turnoff\n\
        \  END-OF-EXECUTION\n\
         END-OF-PROGRAM\n" };
    { name = "The harvest";
      task = [ "Pick up every beeper of the field."; "Write harvestrow once, use it four times." ];
      world =
        [ ". . . . . . .";
          "             ";
          ". 1 1 1 1 1 .";
          "             ";
          ". 1 1 1 1 1 .";
          "             ";
          ". 1 1 1 1 1 .";
          "             ";
          "> 1 1 1 1 1 ." ];
      goal = none_left;
      solution =
        "BEGINNING-OF-PROGRAM\n\
        \  DEFINE-NEW-INSTRUCTION turnright AS\n\
        \    ITERATE 3 TIMES turnleft;\n\
        \  DEFINE-NEW-INSTRUCTION harvestrow AS\n\
        \  BEGIN\n\
        \    pickbeeper;\n\
        \    ITERATE 4 TIMES BEGIN move; pickbeeper END\n\
        \  END;\n\
        \  BEGINNING-OF-EXECUTION\n\
        \    move;\n\
        \    ITERATE 2 TIMES\n\
        \    BEGIN\n\
        \      harvestrow; turnleft; move; turnleft;\n\
        \      harvestrow; turnright; move; turnright\n\
        \    END;\n\
        \    turnoff\n\
        \  END-OF-EXECUTION\n\
         END-OF-PROGRAM\n" };
    { name = "The stairs";
      task = [ "Climb the stairs, picking up"; "the beeper on each step." ];
      world =
        [ ". . . . . .";
          "           ";
          ". . . . 1 .";
          "        -  ";
          ". . . 1|. .";
          "      -    ";
          ". . 1|. . .";
          "    -      ";
          ". 1|. . . .";
          "  -        ";
          ">|. . . . ." ];
      goal = none_left;
      solution =
        "BEGINNING-OF-PROGRAM\n\
        \  DEFINE-NEW-INSTRUCTION turnright AS\n\
        \    ITERATE 3 TIMES turnleft;\n\
        \  DEFINE-NEW-INSTRUCTION climbstair AS\n\
        \  BEGIN\n\
        \    turnleft; move; turnright; move\n\
        \  END;\n\
        \  BEGINNING-OF-EXECUTION\n\
        \    ITERATE 4 TIMES\n\
        \    BEGIN\n\
        \      climbstair; pickbeeper\n\
        \    END;\n\
        \    turnoff\n\
        \  END-OF-EXECUTION\n\
         END-OF-PROGRAM\n" };
    { name = "The maze";
      task = [ "Find the beeper, and pick it up."; "Would your program work in another maze?" ];
      world =
        [ ". . . . . .";
          "  -   -    ";
          ". .|. .|. .";
          "- -   - -  ";
          ". . .|. .|.";
          "  - -   - -";
          ".|. . .|1|.";
          "      -    ";
          ". .|. .|. .";
          "  -   -   -";
          "> .|. . . ." ];
      goal = none_left;
      solution =
        "BEGINNING-OF-PROGRAM\n\
        \  DEFINE-NEW-INSTRUCTION turnright AS\n\
        \    ITERATE 3 TIMES turnleft;\n\
        \  { the right-hand rule: the right hand on the wall }\n\
        \  BEGINNING-OF-EXECUTION\n\
        \    WHILE not-next-to-a-beeper DO\n\
        \      IF right-is-clear THEN BEGIN turnright; move END\n\
        \      ELSE IF front-is-clear THEN move\n\
        \      ELSE turnleft;\n\
        \    pickbeeper;\n\
        \    turnoff\n\
        \  END-OF-EXECUTION\n\
         END-OF-PROGRAM\n" } ]

let level_world (l : level) : Karel.world = Karel.world l.world

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  level : int;
  texts : Text_edit.t array; (* each level's program *)
  run : Karel.run option; (* None: the level's world, before Run *)
  running : bool;
  speed : number; (* primitives a second *)
  due : number; (* primitives owed to the clock *)
}

let initial_model : model =
  { level = 0; texts = Array.of_list (List.map (fun _ -> Text_edit.of_string skeleton) levels); run = None;
    running = false; speed = 4.; due = 0. }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let box x y w h : Widget.box = { x; y; w; h }
let names = List.map (fun l -> l.name) levels

let start (m : model) (p : program) : Karel.run =
  Karel.start ~definitions:p.definitions (level_world (List.nth levels m.level)) p.main

let update (computer : computer) (m : model) : model =
  let level = Gui.menu_in computer (box (-250.) 440. 460. 30.) names m.level in
  let m = if level <> m.level then { m with level; run = None; running = false } else m in
  let texts = Array.copy m.texts in
  texts.(m.level) <- Gui.text_area_in computer (box (-250.) 0. 460. 800.) texts.(m.level);
  let m = { m with texts } in
  let program = parse (Text_edit.to_string texts.(m.level)) in
  let ready = Result.is_ok program in
  let run = Gui.button_in ~enabled:ready computer (box (-420.) (-455.) 110. 36.) (if m.running then "Pause" else "Run") in
  let one = Gui.button_in ~enabled:ready computer (box (-300.) (-455.) 110. 36.) "Step" in
  let reset = Gui.button_in computer (box (-180.) (-455.) 110. 36.) "Reset" in
  let solution = Gui.button_in computer (box (-60.) (-455.) 110. 36.) "Solution" in
  let speed = Gui.slider_in computer (box 280. (-455.) 200. 36.) ~from:1. ~to_:60. m.speed in
  let m = { m with speed } in
  let finished (r : Karel.run) = Karel.status r <> Karel.Running in
  if solution then (
    let texts = Array.copy m.texts in
    texts.(m.level) <- Text_edit.of_string (List.nth levels m.level).solution;
    { m with texts; run = None; running = false })
  else if reset then { m with run = None; running = false }
  else
    match program with
    | Error _ -> { m with running = false }
    | Ok p ->
        if run && m.running then { m with running = false }
        else if run then { m with run = Some (start m p); running = true; due = 0. }
        else if one then
          let r = match m.run with Some r when not (finished r) -> r | _ -> start m p in
          { m with run = Some (Karel.step r); running = false }
        else if m.running then
          let due = m.due +. (m.speed /. 60.) in
          let rec go (r : Karel.run) due = if due < 1. || finished r then (r, due) else go (Karel.step r) (due -. 1.) in
          match m.run with
          | Some r ->
              let r, due = go r due in
              { m with run = Some r; due; running = not (finished r) }
          | None -> { m with running = false }
        else m

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let green_ok = rgb 40 150 60
let red_bad = rgb 200 50 40

let view (_computer : computer) (m : model) : shape list =
  let l = List.nth levels m.level in
  let w = match m.run with Some r -> Karel.current r | None -> level_world l in
  let avenues = (String.length (List.hd l.world) + 1) / 2 and streets = (List.length l.world + 1) / 2 in
  let size = Float.min 60. (Float.min (440. /. float_of_int avenues) (560. /. float_of_int streets)) in
  let status, color =
    match parse (Text_edit.to_string m.texts.(m.level)) with
    | Error (line, msg) -> (Printf.sprintf "line %d: %s" line msg, red_bad)
    | Ok _ -> (
        match m.run with
        | None -> ("Press Run.", darkGray)
        | Some r -> (
            let n = Karel.steps r in
            match Karel.status r with
            | Karel.Running -> (Printf.sprintf "%d steps" n, darkGray)
            | Karel.Finished ->
                if l.goal (Karel.current r) then (Printf.sprintf "Done, in %d steps!" n, green_ok)
                else ("Karel turned off, but the task isn't done.", red_bad)
            | Karel.Error msg -> ("Error shutoff: " ^ msg, red_bad)))
  in
  let gui = Gui.draw () in
  [ rectangle (rgb 235 235 240) 1000. 1000.;
    text black 3. "TINY KAREL" |> move 250. 440. ]
  @ List.mapi (fun i line -> text black 1.8 line |> move 250. (385. -. (float_of_int i *. 28.))) l.task
  @ [ Karel.draw ~size w |> move 250. 20.;
      text color 1.8 status |> move 250. (-360.);
      text darkGray 1.5 (Printf.sprintf "%.0f steps a second" m.speed) |> move 280. (-420.) ]
  @ gui

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = game view update initial_model
let main = Playground_platform.run_app app
