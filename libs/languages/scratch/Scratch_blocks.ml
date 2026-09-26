(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scratch_blocks.mli *)

type category = Motion | Looks | Events | Control | Sensing | Operators | Variables | Pen
type shape = Hat | Stack | Cap | C_block | C_cap | Reporter | Predicate
type part = Word of string | Num of string | Text of string | Menu of string | Bool
type spec = { op : string; category : category; shape : shape; lines : part list list }
type arg = Lit of string | Block of block
and block = { op : string; args : arg list; mouths : block list list }
type script = { x : float; y : float; blocks : block list }

let categories = [ Motion; Looks; Events; Control; Sensing; Operators; Variables; Pen ]

let category_name = function
  | Motion -> "Motion"
  | Looks -> "Looks"
  | Events -> "Events"
  | Control -> "Control"
  | Sensing -> "Sensing"
  | Operators -> "Operators"
  | Variables -> "Variables"
  | Pen -> "Pen"

(* "move %n steps" with ["10"]: the words, and the slots taking the
   defaults in turn; | between the lines of a C block *)
let define op category shape template defaults : spec =
  let defaults = ref defaults in
  let next () = match !defaults with d :: rest -> defaults := rest; d | [] -> "" in
  let part w =
    match w with
    | "%n" -> Num (next ())
    | "%s" -> Text (next ())
    | "%m" -> Menu (next ())
    | "%b" -> Bool
    | w -> Word w
  in
  let line l = List.map part (List.filter (( <> ) "") (String.split_on_char ' ' l)) in
  { op; category; shape; lines = List.map line (String.split_on_char '|' template) }

let specs =
  [
    define "motion_movesteps" Motion Stack "move %n steps" [ "10" ];
    define "motion_turnright" Motion Stack "turn right %n degrees" [ "15" ];
    define "motion_turnleft" Motion Stack "turn left %n degrees" [ "15" ];
    define "motion_pointindirection" Motion Stack "point in direction %n" [ "90" ];
    define "motion_pointtowards" Motion Stack "point towards %m" [ "mouse-pointer" ];
    define "motion_gotoxy" Motion Stack "go to x: %n y: %n" [ "0"; "0" ];
    define "motion_glidesecstoxy" Motion Stack "glide %n secs to x: %n y: %n" [ "1"; "0"; "0" ];
    define "motion_changexby" Motion Stack "change x by %n" [ "10" ];
    define "motion_setx" Motion Stack "set x to %n" [ "0" ];
    define "motion_changeyby" Motion Stack "change y by %n" [ "10" ];
    define "motion_sety" Motion Stack "set y to %n" [ "0" ];
    define "motion_ifonedgebounce" Motion Stack "if on edge, bounce" [];
    define "motion_setrotationstyle" Motion Stack "set rotation style %m" [ "left-right" ];
    define "motion_xposition" Motion Reporter "x position" [];
    define "motion_yposition" Motion Reporter "y position" [];
    define "motion_direction" Motion Reporter "direction" [];
    define "looks_sayforsecs" Looks Stack "say %s for %n seconds" [ "Hello!"; "2" ];
    define "looks_say" Looks Stack "say %s" [ "Hello!" ];
    define "looks_switchcostumeto" Looks Stack "switch costume to %m" [ "1" ];
    define "looks_nextcostume" Looks Stack "next costume" [];
    define "looks_changesizeby" Looks Stack "change size by %n" [ "10" ];
    define "looks_setsizeto" Looks Stack "set size to %n %" [ "100" ];
    define "looks_show" Looks Stack "show" [];
    define "looks_hide" Looks Stack "hide" [];
    define "looks_size" Looks Reporter "size" [];
    define "event_whenflagclicked" Events Hat "when flag clicked" [];
    define "event_whenkeypressed" Events Hat "when %m key pressed" [ "space" ];
    define "event_whenthisspriteclicked" Events Hat "when this sprite clicked" [];
    define "event_whenbroadcastreceived" Events Hat "when I receive %m" [ "message1" ];
    define "event_broadcast" Events Stack "broadcast %m" [ "message1" ];
    define "control_wait" Control Stack "wait %n seconds" [ "1" ];
    define "control_repeat" Control C_block "repeat %n" [ "10" ];
    define "control_forever" Control C_cap "forever" [];
    define "control_if" Control C_block "if %b then" [];
    define "control_if_else" Control C_block "if %b then|else" [];
    define "control_wait_until" Control Stack "wait until %b" [];
    define "control_repeat_until" Control C_block "repeat until %b" [];
    define "control_stop" Control Cap "stop %m" [ "all" ];
    define "sensing_touchingobject" Sensing Predicate "touching %m ?" [ "edge" ];
    define "sensing_keypressed" Sensing Predicate "key %m pressed?" [ "space" ];
    define "sensing_mousedown" Sensing Predicate "mouse down?" [];
    define "sensing_mousex" Sensing Reporter "mouse x" [];
    define "sensing_mousey" Sensing Reporter "mouse y" [];
    define "sensing_timer" Sensing Reporter "timer" [];
    define "sensing_resettimer" Sensing Stack "reset timer" [];
    define "operator_add" Operators Reporter "%n + %n" [];
    define "operator_subtract" Operators Reporter "%n - %n" [];
    define "operator_multiply" Operators Reporter "%n * %n" [];
    define "operator_divide" Operators Reporter "%n / %n" [];
    define "operator_random" Operators Reporter "pick random %n to %n" [ "1"; "10" ];
    define "operator_lt" Operators Predicate "%s < %s" [];
    define "operator_equals" Operators Predicate "%s = %s" [];
    define "operator_gt" Operators Predicate "%s > %s" [];
    define "operator_and" Operators Predicate "%b and %b" [];
    define "operator_or" Operators Predicate "%b or %b" [];
    define "operator_not" Operators Predicate "not %b" [];
    define "operator_join" Operators Reporter "join %s %s" [ "hello "; "world" ];
    define "operator_mod" Operators Reporter "%n mod %n" [];
    define "operator_round" Operators Reporter "round %n" [];
    define "data_setvariableto" Variables Stack "set %m to %s" [ "score"; "0" ];
    define "data_changevariableby" Variables Stack "change %m by %n" [ "score"; "1" ];
    define "data_variable" Variables Reporter "%m" [ "score" ];
    define "pen_clear" Pen Stack "clear" [];
    define "pen_stamp" Pen Stack "stamp" [];
    define "pen_pendown" Pen Stack "pen down" [];
    define "pen_penup" Pen Stack "pen up" [];
    define "pen_setpencolorto" Pen Stack "set pen color to %n" [ "0" ];
    define "pen_changepencolorby" Pen Stack "change pen color by %n" [ "10" ];
    define "pen_setpensizeto" Pen Stack "set pen size to %n" [ "1" ];
    define "pen_changepensizeby" Pen Stack "change pen size by %n" [ "1" ];
  ]

let spec op = List.find (fun (s : spec) -> s.op = op) specs
let slots (s : spec) = List.filter (function Word _ -> false | _ -> true) (List.concat s.lines)

let make op =
  let s = spec op in
  let arg = function Num d | Text d | Menu d -> Lit d | Bool | Word _ -> Lit "" in
  let mouths = match s.shape with C_block | C_cap -> List.map (fun _ -> []) s.lines | _ -> [] in
  { op; args = List.map arg (slots s); mouths }

let variable name = { op = "data_variable"; args = [ Lit name ]; mouths = [] }
