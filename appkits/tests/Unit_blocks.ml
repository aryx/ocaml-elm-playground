(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/blocks: the block editor -- Block_layout.mli's worked
 * example, a C block as tall as its mouth, the places a stack snaps
 * and those it may not, the innermost block under the mouse; and
 * Block_edit.mli's worked example, a reporter out of its slot and
 * back, a slot typed in. *)

open Scratch_blocks
open Block_layout
module E = Block_edit

let t = Testo.create
(* characters 7 wide *)
let measure s = 7. *. float_of_int (String.length s)

let script ?(x = 0.) ?(y = 0.) text =
  match Scratch_text.parse text with Ok [ s ] -> { s with x; y } | _ -> Alcotest.fail ("one script: " ^ text)

let blocks text = (script text).blocks
let size_of text = size ~measure (List.hd (blocks text))

let test_worked_example () =
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "move (10) steps" (111., 28.) (size_of "move (10) steps");
  Alcotest.(check bool) "a reporter in a slot widens the block" true (fst (size_of "say (join [hello ] [world])") > fst (size_of "say [Hello!]"));
  Alcotest.(check (float 1e-9)) "an empty forever: a line, an empty mouth, an arm" 56. (snd (size_of "forever\nend"));
  Alcotest.(check (float 1e-9)) "with a move in it" 70. (snd (size_of "forever\n  move (10) steps\nend"))

let test_snap () =
  let scripts = [ script "when flag clicked\nforever\nend"; script ~x:300. "move (10) steps" ] in
  let move = blocks "turn right (15) degrees" in
  Alcotest.(check bool) "under the hat" true (snap ~measure scripts move ~at:(5., -45.) = Some (Below { script = 0; steps = [ At 0 ] }));
  Alcotest.(check bool) "in the forever's mouth" true (snap ~measure scripts move ~at:(arm +. 3., -70.) = Some (In_mouth ({ script = 0; steps = [ At 1 ] }, 0)));
  Alcotest.(check bool) "nothing under a forever" true (snap ~measure scripts move ~at:(0., -98.) = None);
  Alcotest.(check bool) "above the move, which has no hat" true (snap ~measure scripts move ~at:(300., 28.) = Some (Above 1));
  Alcotest.(check bool) "a hat only starts a script" true (snap ~measure scripts (blocks "when flag clicked") ~at:(5., -45.) = None)

let test_block_at () =
  let scripts = [ script "forever\n  move (10) steps\nend" ] in
  let pieces, _ = layout ~measure scripts in
  Alcotest.(check bool) "in the mouth: the move" true (block_at pieces (30., -40.) = Some { script = 0; steps = [ At 0; Mouth 0; At 0 ] });
  Alcotest.(check bool) "on the arm: the forever" true (block_at pieces (5., -40.) = Some { script = 0; steps = [ At 0 ] });
  Alcotest.(check bool) "the move's slot" true (slot_at pieces (14. +. 8. +. 28. +. 4. +. 5., -40.) = Some { script = 0; steps = [ At 0; Mouth 0; At 0; Arg 0 ] })

let test_edit () =
  let scripts = [ script "when flag clicked\nmove (10) steps\nturn right (15) degrees" ] in
  (match E.take scripts { script = 0; steps = [ At 1 ] } with
  | Some (E.Stack taken, rest) ->
      Alcotest.(check int) "the move and all under it" 2 (List.length taken);
      Alcotest.(check int) "the hat left" 1 (List.length (List.hd rest).blocks);
      Alcotest.(check bool) "dropped back under the hat" true (E.drop ~measure rest taken (Below { script = 0; steps = [ At 0 ] }) = scripts)
  | _ -> Alcotest.fail "a stack");
  let scripts = [ script "say (join [a] [b])" ] in
  let slot = { script = 0; steps = [ At 0; Arg 0 ] } in
  (match E.take scripts slot with
  | Some (E.Reporter r, rest) ->
      Alcotest.(check (option string)) "the slot emptied" (Some "") (E.text rest slot);
      Alcotest.(check bool) "put back" true (E.drop_in_slot rest slot r = scripts)
  | _ -> Alcotest.fail "a reporter");
  let typed = E.set_text scripts { script = 0; steps = [ At 0; Arg 0; Arg 1 ] } "c" in
  Alcotest.(check string) "a slot inside a reporter typed in" "say (join [a] [c])\n" (Scratch_text.print typed)

(* Snap!'s rings: a stack into the mouth of the command ring in a
   run's slot, and out again; a reporter into a reporter ring's slot *)
let test_rings () =
  let scripts = [ script "run ({ })" ] in
  let ring = { script = 0; steps = [ At 0; Arg 0 ] } in
  let _, targets = layout ~measure scripts in
  let at = List.assoc (In_mouth (ring, 0)) targets in
  let move = blocks "move (10) steps" in
  Alcotest.(check bool) "the ring's mouth, a place for a stack" true (snap ~measure scripts move ~at = Some (In_mouth (ring, 0)));
  let scripts' = E.drop ~measure scripts move (In_mouth (ring, 0)) in
  Alcotest.(check string) "in the ring" "run ({ move (10) steps })\n" (Scratch_text.print scripts');
  (match E.take scripts' { script = 0; steps = [ At 0; Arg 0; Mouth 0; At 0 ] } with
  | Some (E.Stack [ { op = "motion_movesteps"; _ } ], rest) -> Alcotest.(check bool) "and out" true (rest = scripts)
  | _ -> Alcotest.fail "the move");
  let scripts = [ script "say (map ({ [] }) over (numbers from (1) to (3)))" ] in
  let inner = { script = 0; steps = [ At 0; Arg 0; Arg 0; Arg 0 ] } in
  let pieces, _ = layout ~measure scripts in
  Alcotest.(check bool) "the ring's slot is a slot" true (List.exists (function Slot { path; _ } -> path = inner | _ -> false) pieces);
  let times = List.hd (blocks "say (() * ())") in
  let r = match times.args with [ Block r ] -> r | _ -> Alcotest.fail "a reporter" in
  Alcotest.(check string) "a reporter in the ring" "say (map ({ (() * ()) }) over (numbers from (1) to (3)))\n" (Scratch_text.print (E.drop_in_slot scripts inner r))

let tests =
  [
    t "blocks: Block_layout.mli's worked example, and sizes" test_worked_example;
    t "blocks: where a stack snaps, and where not" test_snap;
    t "blocks: the innermost block, the slot" test_block_at;
    t "blocks: Block_edit.mli's worked example, a reporter, a slot" test_edit;
    t "blocks: Snap!'s rings, a stack and a reporter in them" test_rings;
  ]
