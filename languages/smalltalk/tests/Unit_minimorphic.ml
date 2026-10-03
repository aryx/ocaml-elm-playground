(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_minimorphic.mli *)

module I = St_interp

let check = Alcotest.(check string)

(* a system of its own, whose mouse the test moves: x, y, buttons *)
let boot () : I.vm * (int * int * int) ref =
  let mouse = ref (0, 0, 0) in
  let host = { St_boot.quiet_host with mouse = (fun () -> !mouse) } in
  (St_boot.boot ~host ~kernel:St_kernel.mini_morphic (), mouse)

let print (vm : I.vm) (text : string) : string =
  match I.evaluate vm text with Ok v -> I.print_string vm v | Error e -> "error: " ^ e

(* the Display's pixels at a few points, as a string of 0 and 1 *)
let pixels (vm : I.vm) (points : (int * int) list) : string =
  String.concat "" (List.map (fun (x, y) -> print vm (Printf.sprintf "Display pixelAt: %d @ %d" x y)) points)

let tests =
  Testo.categorize "MiniMorphic"
    [
      Testo.create "a morph drawn: its colour and its frame, the world's white around" (fun () ->
          let vm, _ = boot () in
          ignore (print vm "Smalltalk at: #W put: (WorldMorph on: Display). Smalltalk at: #M put: Morph new");
          check "added" "100@80 corner: 150@120" (print vm "M color: #black. W addMorph: M. M position: 100 @ 80. M bounds");
          check "nothing drawn before the cycle" "00" (pixels vm [ (120, 100); (10, 10) ]);
          ignore (print vm "W doOneCycle");
          check "inside black, outside white, the hand at the mouse black" "1001" (pixels vm [ (120, 100); (160, 100); (120, 130); (3, 3) ]);
          check "a gray morph: its frame black, every other pixel inside" "1101"
            (ignore (print vm "M color: #gray. W doOneCycle");
             pixels vm [ (100, 80); (102, 82); (103, 82); (149, 119) ]));
      Testo.create "damage: what changed is remembered, merged, and all that is redrawn" (fun () ->
          let vm, _ = boot () in
          ignore (print vm "Smalltalk at: #W put: (WorldMorph on: Display). Smalltalk at: #M put: Morph new. W addMorph: M. W doOneCycle");
          check "nothing to redraw" "0" (print vm "W damage size");
          check "a move: the place left and the place taken" "OrderedCollection (0@0 corner: 50@40 10@5 corner: 60@45 )"
            (print vm "M position: 10 @ 5. W damage");
          check "few rectangles: each redrawn" "4" (print vm "M position: 300 @ 200. W damage size");
          (* a pixel scribbled outside the damage stays: nothing there is redrawn *)
          ignore (print vm "Display fill: (700 @ 500 corner: 702 @ 502) rule: 15. W doOneCycle");
          check "the morph moved, the old place white, the scribble untouched" "101" (pixels vm [ (320, 220); (20, 20); (700, 500) ]);
          check "many: the one that holds them all" "OrderedCollection (100@80 corner: 550@440 )"
            (print vm "1 to: 5 do: [:i | M position: i * 100 @ (i * 80)]. W damageToRedraw");
          ignore (print vm "W invalidRect: W bounds. W doOneCycle");
          check "the whole world redrawn: gone" "0" (pixels vm [ (700, 500) ]));
      Testo.create "step: an atom moves each cycle and bounces off the walls" (fun () ->
          let vm, _ = boot () in
          ignore (print vm "Smalltalk at: #W put: (WorldMorph on: Display). Smalltalk at: #A put: AtomMorph new. W addMorph: A. A position: 780 @ 300. A velocity: 5 @ 2");
          check "a cycle" "785@302" (print vm "W doOneCycle. A position");
          check "the wall: 800 wide, the atom 12" "780@304" (print vm "W doOneCycle. A position");
          check "its velocity turned back" "-5@2" (print vm "A velocity");
          check "its frame drawn where it is, erased where it was" "10" (pixels vm [ (780, 310); (795, 310) ]));
      Testo.create "the hand: the button picks up the morph under it, moves it, puts it down in front" (fun () ->
          let vm, mouse = boot () in
          ignore (print vm "Smalltalk at: #W put: (WorldMorph on: Display). Smalltalk at: #M put: Morph new. Smalltalk at: #N put: Morph new");
          ignore (print vm "W addMorph: M. M position: 100 @ 100. W addMorph: N. N position: 400 @ 300. W doOneCycle");
          mouse := (110, 110, 4);
          check "picked up" "a HandMorph" (print vm "W doOneCycle. M owner");
          check "the frontmost under a point" "true" (print vm "(W morphAt: 410 @ 310) == N");
          mouse := (410, 310, 4);
          check "carried: it keeps its place under the hand" "400@300" (print vm "W doOneCycle. M position");
          mouse := (410, 310, 0);
          check "put down, in the world" "a WorldMorph" (print vm "W doOneCycle. M owner");
          check "in front of the other" "true" (print vm "(W morphAt: 440 @ 330) == M");
          check "the place it left is white again" "0" (pixels vm [ (120, 120) ]));
      Testo.create "fifty bouncing atoms: a hundred cycles, every atom still in the box" (fun () ->
          let vm, _ = boot () in
          ignore (print vm "Smalltalk at: #W put: (WorldMorph bouncingAtoms: 50)");
          check "a hundred cycles" "50" (print vm "100 timesRepeat: [W doOneCycle]. W submorphs size");
          check "all inside" "true"
            (print vm "W submorphs allSatisfy: [:a | (a bounds intersect: W bounds) = a bounds]");
          check "one atom stepping: two small rectangles to redraw" "true"
            (print vm "W submorphs first step. (W damageToRedraw inject: 0 into: [:sum :r | sum + r area]) < 1000"));
    ]
