(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinySnap: Scratch with the rest of computer science put back (Snap!,
 * Jens Monig and Brian Harvey, UC Berkeley, 2011; BYOB, "Build Your Own
 * Blocks", 2008; in the look of Snap! 4, 2015).
 *
 * Scratch (TinyScratch) leaves out, on purpose, what a child of eight
 * does not need: procedures, functions as values, data structures.
 * Brian Harvey, who had taught Scheme from SICP at Berkeley for
 * decades, wanted them back, in blocks, for "The Beauty and Joy of
 * Computing", the course that teaches it -- and Snap! is Scheme with
 * Scratch's face:
 *
 * - **blocks of your own**: Make a block (in Other) puts a "define"
 *   hat in the scripts area; type its kind (command, reporter,
 *   predicate) and its template ("tree %size": size is a parameter),
 *   hang the body under it, and the new block is in the palette.
 *   Recursion comes free: the tree draws itself with two smaller trees
 *   (libs/languages/scratch, Scratch_blocks.mli);
 * - **rings, the lambda**: a grey ring round a reporter or a script
 *   makes it a value -- to call, to run, to keep in a variable, to give
 *   to another block. Its empty slots are its parameters: map
 *   ({ (() * ()) }) over a list squares each item, the one input
 *   filling both slots. A ring keeps the variables it was made with,
 *   a closure (Scratch_run.mli);
 * - **first-class lists**: made, kept in variables, passed, reported;
 *   map, keep and combine, the higher-order functions; a list said by
 *   a sprite shown as Snap! shows it, a table.
 *
 * The project it opens: the turtle (Snap!'s sprite with no costume)
 * draws a tree by a recursive custom command, then says the squares of
 * one to six, by map; click it and it says 10 factorial, by a
 * recursive custom reporter; click the loose combine and it says 55,
 * Gauss's sum. Click the green flag.
 *
 * Everything else is TinyScratch's (drag, drop, click, type; the keys
 * to the project; the scripts as text under the sprites): the same
 * environment, Scratch_ide, with Snap!'s layout -- the stage on the
 * right, the palette on the left, dark -- and Snap!'s colours, with its
 * zebra colouring (a block in a block of its colour, lighter).
 *
 * What it uses: libs/languages/scratch (Scratch_blocks, Scratch_text,
 * Scratch_run, Snap!'s blocks and runtime included), appkits/blocks
 * (Block_layout, Block_edit, rings and all), Scratch_ide and
 * Scratch_look.
 *
 * What it deliberately does not do: the Block Editor, Snap!'s dialog
 * (here the definition is a script like any other, its template
 * typed); input names on rings (only empty slots); variadic inputs
 * (the list block's three slots, the empty ones at its end no items);
 * first-class sprites and costumes, continuations, JavaScript
 * functions; "warp" and a reporter yielding in its loops (Snap!'s run
 * as processes of their own; here a reporter runs to its value at
 * once); the list watcher's editing; the cloud.
 *
 * Exercises: a garbage collector for the heap of lists and cells --
 * mark from the variables, the threads' environments and the rings,
 * sweep; input names on rings ("input names: a b"); the list block
 * made variadic, with its arrows; "report" in a command block as
 * "stop this block".
 *)
open Playground
module B = Scratch_blocks
module R = Scratch_run
module Ide = Scratch_ide

(*****************************************************************************)
(* The project *)
(*****************************************************************************)

let definitions =
  {|define [command v] [tree %size]
if <(size) > (8)> then
  move (size) steps
  turn left (25) degrees
  tree ((size) * (0.7))
  turn right (50) degrees
  tree ((size) * (0.7))
  turn left (25) degrees
  move ((0) - (size)) steps
end

define [reporter v] [factorial %n]
if <(n) < (2)> then
  report (1)
end
report ((n) * (factorial ((n) - (1))))|}

let scripts =
  {|when flag clicked
clear
go to x: (0) y: (-150)
point in direction (0)
set pen color to (70)
pen down
tree (70)
pen up
say (map ({ (() * ()) }) over (numbers from (1) to (6)))

when this sprite clicked
say (factorial (10))

(combine (numbers from (1) to (10)) using ({ (() + ()) }))|}

(*****************************************************************************)
(* Snap!'s screen *)
(*****************************************************************************)

let is_hat (s : B.spec) = s.shape = B.Hat

(* the custom blocks the sprites' "define" scripts make *)
let customs (stage : R.t) =
  List.concat_map
    (fun (s : R.sprite) ->
      List.filter_map
        (fun (sc : B.script) ->
          match sc.blocks with { op = "procedures_definition"; args = [ Lit kind; Lit template ]; _ } :: _ -> Some (B.custom_op kind template, B.params template) | _ -> None)
        s.scripts)
    stage.sprites

(* the palette: Snap!'s categories -- the hats in Control, the lists'
   blocks, the custom ones in Other; a reporter per variable and per
   parameter *)
let palette (stage : R.t) category =
  let defined = customs stage in
  let variables = List.sort_uniq compare (("score" :: List.map fst stage.vars) @ List.concat_map snd defined) in
  let of_spec (s : B.spec) =
    let shown = s.category = category || (category = B.Control && s.category = B.Events) in
    if not shown || s.op = "procedures_definition" then []
    else if s.op = "data_variable" then List.map B.variable variables
    else [ B.make s.op ]
  in
  List.concat_map of_spec (B.specs @ B.snap_specs) @ if category = B.Other then List.map (fun (op, _) -> B.make op) defined else []

let config : Ide.config =
  {
    title = "Snap!";
    menus = "Build Your Own Blocks";
    bar = rgb 40 40 46;
    theme = Scratch_look.snap;
    colors =
      {
        pane = rgb 52 52 58;
        header = rgb 40 40 46;
        scripts = rgb 88 88 96;
        side = rgb 62 62 70;
        sheet = rgb 44 44 50;
        line = rgb 100 100 110;
        ink = rgb 225 225 232;
        sheet_ink = rgb 205 212 222;
        button_off = rgb 72 72 80;
      };
    categories = B.snap_categories;
    palette;
    make_block = true;
    stage = { cx = 328.; cy = 339.; k = 0.7 };
    palette_x = (-500., -265.);
    scripts_x = (-265., 160.);
    flag = (400., 485.);
    stop = (440., 485.);
    name_at = None;
  }

let project () =
  let at x y (s : B.script) = { s with x; y } in
  (* read together, the calls needing the definitions; in a column,
     the scripts that use the definitions first *)
  let scripts =
    match Ide.column config (definitions ^ "\n\n" ^ scripts) with
    | [ tree; factorial; flag; click; combine ] -> [ at (-250.) 440. flag; at (-250.) 140. click; at (-250.) 72. combine; at (-250.) 20. factorial; at (-250.) (-188.) tree ]
    | scripts -> scripts
  in
  let turtle = R.sprite ~name:"Turtle" ~costumes:1 ~radius:14. scripts in
  R.stage [ { turtle with direction = 0.; y = -150. } ]

let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (Ide.app config (project ()) ~current:"Turtle"))
