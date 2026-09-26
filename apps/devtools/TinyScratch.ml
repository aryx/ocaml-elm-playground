(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyScratch: programming by snapping blocks together (Scratch,
 * Mitchel Resnick, John Maloney, Natalie Rusk, Evelyn Eastmond, Amon
 * Millner and the Lifelong Kindergarten group, MIT Media Lab, 2007;
 * this is the look of Scratch 2.0, 2013, the one in the browser).
 *
 * Scratch is Logo's grandchild (Seymour Papert, 1967: the turtle is a
 * sprite with a pen) by way of Etoys (Alan Kay, Squeak, 1997, where
 * scripts were tiles dragged onto objects) -- and the first Scratch
 * was itself written in Squeak, Smalltalk's heir (TinySmalltalk80).
 * What it added is what made tens of millions of children program:
 *
 * - **no syntax errors**: a program is blocks whose shapes say where
 *   they fit -- a notch above, a tab below, a mouth, a round or a
 *   pointed slot -- so every program that can be built can run
 *   (libs/languages/scratch, Scratch_blocks.mli). The blocks are one
 *   table, which the palette, the editor, the runtime and the text
 *   all read;
 * - **tinkering**: click any script, even while the project runs, and
 *   it runs; change a number, click again. Nothing is compiled, nothing
 *   is saved first;
 * - **concurrency for free**: every script is a thread, started by its
 *   hat (the green flag, a key, a click, a message), and a thread
 *   yields only at the end of a loop's turn or in a wait, then the
 *   stage is drawn -- so a forever loop is an animation and two sprites
 *   dancing together need no locks (Scratch_run.mli);
 * - **the stage**: sprites on a 480 x 360 plane, x right and y up,
 *   directions a compass's, the pen of Logo's turtle.
 *
 * The text under the sprites is the current sprite's scripts in the
 * scratchblocks notation (Scratch_text.mli), the forums' way of
 * writing blocks, printed from the blocks as they change: the same
 * program as text, which is what TinyBasic and TinyTurboPascal have
 * instead of blocks.
 *
 * The project it opens is the first one children make: the cat that
 * walks and bounces, and says hello; and a pencil sprite drawing a
 * flower of squares, a turtle-graphics classic, at the same time.
 * Click the green flag.
 *
 * Mouse: drag a block from the palette into the scripts area (a stack
 * snaps under a block, into a mouth, or above a script; a reporter
 * into a slot); drag a block in a script and it takes those under it
 * along; drop anything on the palette to delete it; click a script to
 * run it; click a slot and type (Enter). The wheel scrolls the palette
 * or the scripts. Click a sprite on the stage for "when this sprite
 * clicked", a thumbnail below the stage to edit its scripts. Keys go
 * to the project ("when [space] key pressed", "key [space] pressed?").
 * Flags: sprite=pencil opens the pencil's scripts; run=on clicks the
 * green flag at the start.
 *
 * What it uses: libs/languages/scratch (Scratch_blocks, Scratch_text,
 * Scratch_run), appkits/blocks (Block_layout, Block_edit), and the
 * environment it shares with TinySnap, Scratch_ide (the panes, the
 * mouse, the keys) and Scratch_look (the blocks and the stage drawn),
 * to which this file gives Scratch 2's layout, colours and palette.
 * Not gui/: the blocks are drawn from Block_layout's pieces, the panes
 * by hand.
 *
 * The trick of this app, a departure from Scratch: the stage steps
 * every other frame of the Playground (Scratch 2's 30 a second), its
 * clock counting those steps rather than reading the time, so that a
 * run is the same run every time -- wait and glide included.
 *
 * What it deliberately does not do: sounds (the Sound category and the
 * cat's meow); the paint editor, costumes drawn by the user (the
 * costumes here are shapes); the stage's own scripts and backdrops;
 * lists; clones (Scratch 2's); "ask and wait"; the "more blocks" of
 * Scratch 2, procedures of one's own (BYOB, then Snap!, went further:
 * blocks as first-class values, lambda); saving and sharing projects
 * -- the online community that was half of Scratch.
 *
 * Exercises: save the project in the store as scratchblocks text, one
 * section a sprite, and load it back (Scratch_text reads it already);
 * a context menu on a block: duplicate, delete; right-click a
 * reporter in the palette to see its value, as Scratch shows in a
 * bubble; "ask [] and wait" with a text field on the stage; clones.
 *)
open Playground
module B = Scratch_blocks
module R = Scratch_run
module Ide = Scratch_ide

(*****************************************************************************)
(* The project *)
(*****************************************************************************)

let cat_scripts =
  {|when flag clicked
go to x: (-120) y: (-80)
point in direction (90)
say [Hello!] for (1) seconds
forever
  move (4) steps
  next costume
  if on edge, bounce
end

when this sprite clicked
change size by (10)
say (join [I am ] (size)) for (1) seconds

when [space v] key pressed
turn right (15) degrees|}

let pencil_scripts =
  {|when flag clicked
clear
go to x: (0) y: (20)
point in direction (90)
set pen color to (0)
pen down
repeat (36)
  repeat (4)
    move (70) steps
    turn right (90) degrees
  end
  turn right (10) degrees
  change pen color by (5)
end
pen up|}

(*****************************************************************************)
(* Scratch 2's screen *)
(*****************************************************************************)

(* the palette: a category's blocks, a reporter per variable *)
let palette (stage : R.t) category =
  let variables = "score" :: List.filter (( <> ) "score") (List.map fst stage.vars) in
  List.concat_map
    (fun (s : B.spec) ->
      if s.category <> category then []
      else if s.op = "data_variable" then List.map B.variable (List.sort_uniq compare variables)
      else [ B.make s.op ])
    B.specs

(* the stage top left, the sprites under it, the palette in the
   middle, the scripts on the right *)
let config : Ide.config =
  {
    title = "TinyScratch";
    menus = "File   Edit   Tips";
    bar = rgb 37 160 224;
    theme = Scratch_look.scratch2;
    colors =
      {
        pane = white;
        header = rgb 230 232 235;
        scripts = rgb 242 242 242;
        side = rgb 230 232 235;
        sheet = white;
        line = rgb 200 200 205;
        ink = rgb 90 90 95;
        sheet_ink = rgb 40 40 40;
        button_off = white;
      };
    categories = B.categories;
    palette;
    make_block = false;
    stage = { cx = -315.; cy = 300.; k = 0.75 };
    palette_x = (-130., 95.);
    scripts_x = (95., 500.);
    flag = (-200., 452.);
    stop = (-160., 452.);
    name_at = Some (-455., 452.);
  }

let project () =
  let cat = R.sprite ~name:"Cat" ~costumes:2 ~radius:34. (Ide.column config cat_scripts) in
  let pencil = R.sprite ~name:"Pencil" ~costumes:1 ~radius:14. (Ide.column config pencil_scripts) in
  R.stage [ { cat with x = -120.; y = -80. }; { pencil with x = 0.; y = 20.; pen_hue = 0. } ]

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) (Ide.app config (project ()) ~current:"Cat")
