(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_morphic.mli *)

let check = Alcotest.(check string)

open Testutil_morphic

let tests =
  Testo.categorize "Squeak Morphic"
    [
      Testo.create "shapes: a rectangle and its border, an ellipse and its corners" (fun () ->
          let w = boot () in
          check "the world's gray" "Color(204 204 204)" (colour w 10 10);
          ignore (print w "Smalltalk at: #R put: RectangleMorph new. R color: Color green. W addMorph: R. R position: 100 @ 50. W doOneCycle");
          check "its border" "Color(0 0 0)" (colour w 100 50);
          check "its inside" "Color(0 255 0)" (colour w 110 60);
          ignore (print w "Smalltalk at: #E put: EllipseMorph new. E extent: 60 @ 40. W addMorph: E. E position: 200 @ 100. W doOneCycle");
          check "the ellipse's middle" "Color(255 255 0)" (colour w 230 120);
          check "its corner is the world's" "Color(204 204 204)" (colour w 201 101);
          check "and a click there is not on it" "#(true false)"
            (print w "Array with: (E containsPoint: 230 @ 120) with: (E containsPoint: 201 @ 101)");
          check "the frontmost at a point" "true" (print w "(W morphAt: 230 @ 120) == E"));
      Testo.create "the hand: a morph nobody claims is picked up, carried over its shadow, put down in front" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #R put: RectangleMorph new. W addMorph: R. R position: 100 @ 50. Smalltalk at: #S put: RectangleMorph new. W addMorph: S. S position: 300 @ 200");
          move w 110 60 4;
          check "picked up" "a HandMorph" (print w "R owner");
          move w 150 100 4;
          check "it keeps its place under the hand" "140@90" (print w "R position");
          (* 204 under black of alpha 77 (3/10): (204 * 178 + 127) / 255 *)
          check "its shadow on the world: the gray darkened by a black glass" "Color(142 142 142)" (colour w 192 132);
          move w 310 210 4;
          move w 310 210 0;
          check "put down, in the world" "a PasteUpMorph" (print w "R owner");
          check "in front of the other" "true" (print w "(W morphAt: 320 @ 215) == R");
          check "where it was is the world's again" "Color(204 204 204)" (colour w 110 60));
      Testo.create "events: the morph that wants the mouse gets it, down, move and up" (fun () ->
          let w = boot () in
          ignore
            (print w
               "Smalltalk at: #B put: (SimpleButtonMorph new label: 'Go'; yourself). Smalltalk at: #Hits put: OrderedCollection new. B target: Hits selector: #removeFirst. Hits add: 1; add: 2. W addMorph: B. B position: 50 @ 50");
          move w 55 55 4;
          check "not picked up: it has the mouse" "true" (print w "B owner == W and: [W hand mouseFocus == B]");
          move w 55 55 0;
          check "the click sent its message" "1" (print w "Hits size");
          check "a click on its label is a click on it" "0" (click w 62 58; print w "Hits size");
          move w 55 55 4;
          move w 300 200 0;
          check "the button let go elsewhere: no click, no error" "0" (print w "Hits size"));
      Testo.create "the halo: the blue button's handles delete, pick up, duplicate and resize any morph" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #R put: RectangleMorph new. W addMorph: R. R position: 100 @ 100");
          click ~button:1 w 110 110;
          check "a halo around it" "true" (print w "W submorphs first isHalo and: [W submorphs first target == R]");
          check "its bounds, 14 around" "86@86 corner: 164@154" (print w "W submorphs first bounds");
          (* resize: the handle at the bottom right, dragged *)
          move w 157 147 4;
          move w 200 180 4;
          move w 200 180 0;
          check "resized to where the hand went" "100@100 corner: 200@180" (print w "R bounds");
          check "the halo followed" "86@86 corner: 214@194" (print w "W submorphs first bounds");
          (* duplicate: the top right *)
          move w 207 93 4;
          check "a copy in the hand" "true" (print w "W hand submorphs first ~~ R and: [W hand submorphs first class == RectangleMorph]");
          move w 300 93 0;
          check "two of them in the world" "2" (print w "(W submorphs select: [:m | m class == RectangleMorph]) size");
          (* delete: the top left of the copy's halo *)
          check "the halo is the copy's" "true" (print w "(W submorphs detect: [:m | m isHalo]) target ~~ R");
          ignore (print w "Smalltalk at: #P put: (W submorphs detect: [:m | m isHalo]) bounds origin + 7");
          let px = int_of_string (print w "P x") and py = int_of_string (print w "P y") in
          click w px py;
          check "the copy and its halo gone" "#(1 0)"
            (print w "Array with: (W submorphs select: [:m | m class == RectangleMorph]) size with: (W submorphs select: [:m | m isHalo]) size"));
      Testo.create "text: a click gives the keys, characters go in at the caret" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #T put: TextMorph new. W addMorph: T. T position: 50 @ 50");
          typed w "lost";
          check "no focus: the keys go nowhere" "" (print w "T contents");
          click w 60 60;
          check "it has the keys" "true" (print w "T hasFocus");
          typed w "Helo";
          typed w "\028l\029\r2";
          check "typed, an arrow back, a letter, an arrow, a return" "Hello|2"
            (String.map (fun c -> if c = '\r' then '|' else c) (print w "T contents"));
          typed w "\b3";
          check "a backspace" "3" (print w "T contents last printString" |> fun s -> String.sub s 1 1);
          check "the caret: on the second line, after one character" "true"
            (print w "T cursorPoint = ((53 + (StrikeFont default widthOf: $3)) @ (52 + StrikeFont default height))");
          (* a click in the first line, after the two first letters *)
          ignore (print w "Smalltalk at: #X put: 53 + (StrikeFont default widthOfString: 'He') + 1");
          click w (int_of_string (print w "X")) 55;
          check "the caret where the click was" "2" (print w "T cursor");
          check "ink where the H is, paper beside" "true" (print w "(F colorAt: 60 @ 60) = Color white or: [(F colorAt: 60 @ 60) = Color black]"));
      Testo.create "a window: dragged by its title, its panes follow its size, its box closes it, a copy is whole" (fun () ->
          let w = boot () in
          ignore
            (print w
               "Smalltalk at: #Win put: SystemWindow new. Win labelString: 'Workspace'. Smalltalk at: #T put: TextMorph new. Win addMorph: T frame: (0 @ 0 corner: 1/2 @ 1). W addMorph: Win. Win position: 20 @ 20. W doOneCycle");
          check "the pane: the left half, under the title" "21@41 corner: 150@179" (print w "T bounds");
          ignore (print w "Win extent: 300 @ 200");
          check "the window bigger: the pane too" "21@41 corner: 170@219" (print w "T bounds");
          move w 150 28 4;
          check "its title does not want the mouse: the window is picked up" "a HandMorph" (print w "Win owner");
          move w 180 58 4;
          move w 180 58 0;
          check "moved with all it holds" "51@71" (print w "T position");
          check "a copy has its own pane and label" "true"
            (print w "| c | c := Win duplicate. c submorphs size = 3 and: [(c submorphs includes: T) not and: [c labelString: 'Copy'. (Win submorphs anySatisfy: [:m | (m isKindOf: StringMorph) and: [m contents = 'Workspace']])]]");
          click w 60 60;
          check "its box closes it" "nil" (print w "Win owner"));
      Testo.create "the world's menu: a click on the world, an item, a new morph in the hand, a click to put it down" (fun () ->
          let w = boot () in
          click w 100 100;
          check "a menu" "true" (print w "W submorphs first isMenu");
          ignore (print w "Smalltalk at: #P put: (W submorphs first submorphs at: 2) bounds origin + 3");
          let px = int_of_string (print w "P x") and py = int_of_string (print w "P y") in
          click w px py;
          check "the menu gone, an ellipse in the hand" "#(0 #EllipseMorph)"
            (print w "Array with: W submorphs size with: W hand submorphs first class name");
          move w 250 200 0;
          click w 250 200;
          check "put down where the click was" "250@200" (print w "W submorphs first position");
          click w 20 20;
          click w 380 280;
          check "a menu goes at a click elsewhere, where another opens" "1" (print w "(W submorphs select: [:m | m isMenu]) size"));
      Testo.create "step and damage: an atom bounces in the world, only where it was and is are redrawn" (fun () ->
          let w = boot () in
          ignore (print w "Smalltalk at: #A put: AtomMorph new. W addMorph: A. A position: 380 @ 100. A velocity: 5 @ 2. W doOneCycle");
          check "a cycle" "385@102" (print w "A position");
          check "the wall: 400 wide, the atom 14" "380@104" (print w "W doOneCycle. A position");
          ignore (print w "F fill: (10 @ 10 corner: 12 @ 12) color: Color red. W doOneCycle");
          check "a scribble outside the damage stays" "Color(255 0 0)" (colour w 10 10);
          check "until the world is redrawn" "Color(204 204 204)" (ignore (print w "W restoreDisplay. W doOneCycle"); colour w 10 10));
    ]
