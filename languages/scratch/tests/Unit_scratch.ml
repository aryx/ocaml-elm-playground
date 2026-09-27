(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* scratch/: Scratch_text.mli's worked example, the brackets < and >
 * that are also comparisons, a project printed back as it was read,
 * the errors; Scratch_run.mli's worked example, two forevers taking
 * turns, a script without loops run in one frame, Scratch's values,
 * a broadcast heard the frame after, waits, the pen, the edges. *)

open Scratch_blocks
module R = Scratch_run

let t = Testo.create

let parse text = match Scratch_text.parse text with Ok s -> s | Error e -> Alcotest.fail e
let blocks text = (List.hd (parse text)).blocks

let test_worked_example () =
  (match blocks "move (10) steps" with
  | [ { op = "motion_movesteps"; args = [ Lit "10" ]; _ } ] -> ()
  | _ -> Alcotest.fail "move");
  (match blocks "say (join [hi ] (score))" with
  | [ { op = "looks_say"; args = [ Block { op = "operator_join"; args = [ Lit "hi "; Block { op = "data_variable"; args = [ Lit "score" ]; _ } ]; _ } ]; _ } ] -> ()
  | _ -> Alcotest.fail "say (join ...)");
  Alcotest.(check string) "printed back" "say (join [hi ] (score))\n" (Scratch_text.print (parse "say (join [hi ] (score))"))

let test_angles () =
  match blocks "if <(x position) > (100)> then\n  say [far]\nend" with
  | [ { op = "control_if"; args = [ Block { op = "operator_gt"; args = [ Block { op = "motion_xposition"; _ }; Lit "100" ]; _ } ]; mouths = [ [ { op = "looks_say"; _ } ] ] } ] ->
      Alcotest.(check string) "and back" "if <(x position) > (100)> then" (Scratch_text.line (List.hd (blocks "if <(x position) > (100)> then\nend")));
      (match blocks "wait until <<mouse down?> and <not <touching [edge v]?>>>" with
      | [ { args = [ Block { op = "operator_and"; args = [ Block { op = "sensing_mousedown"; _ }; Block { op = "operator_not"; _ } ]; _ } ]; _ } ] -> ()
      | _ -> Alcotest.fail "and, not")
  | _ -> Alcotest.fail "if > then"

let project =
  {|when flag clicked
go to x: (-150) y: (-60)
set rotation style [left-right v]
forever
  move (4) steps
  next costume
  if on edge, bounce
  if <key [space v] pressed?> then
    change y by (10)
  else
    set y to (-60)
  end
end

when this sprite clicked
change size by (10)
say (join [size ] (size)) for (1) seconds
|}

let test_round_trip () =
  let scripts = parse project in
  Alcotest.(check int) "two scripts" 2 (List.length scripts);
  Alcotest.(check string) "printed as written" project (Scratch_text.print scripts);
  Alcotest.(check bool) "an end left out at the bottom is implied" true (blocks "forever\n  move (1) steps" = blocks "forever\n  move (1) steps\nend")

let test_errors () =
  let bad text = match Scratch_text.parse text with Ok _ -> false | Error _ -> true in
  Alcotest.(check bool) "an unknown block" true (bad "fly (10) steps");
  Alcotest.(check bool) "an end with no block" true (bad "move (10) steps\nend");
  Alcotest.(check bool) "a bracket not closed" true (bad "move (10 steps")

(* a stage of the sprites given as texts *)
let stage sprites = R.stage (List.map (fun (name, text) -> R.sprite ~name ~costumes:2 ~radius:20. (parse text)) sprites)

let input time = { R.mouse_x = 0.; mouse_y = 0.; mouse_down = false; keys = []; time }

let frames n t = List.fold_left (fun t i -> R.step (input (float_of_int i /. 30.)) t) t (List.init n (fun i -> i + 1))

let test_run_worked_example () =
  let t = R.green_flag (stage [ ("Cat", "when flag clicked\nrepeat (3)\n  move (10) steps\nend") ]) in
  let xs = List.init 3 (fun i -> (R.find (frames (i + 1) t) "Cat").x) in
  Alcotest.(check (list (float 1e-9))) "a move a frame" [ 10.; 20.; 30. ] xs;
  Alcotest.(check int) "still running after three" 1 (List.length (frames 3 t).threads);
  Alcotest.(check int) "done in the fourth" 0 (List.length (frames 4 t).threads)

let test_turns () =
  let t = R.green_flag (stage [ ("A", "when flag clicked\nforever\n  change x by (1)\nend"); ("B", "when flag clicked\nforever\n  change y by (2)\nend") ]) in
  let t = frames 5 t in
  Alcotest.(check (pair (float 0.) (float 0.))) "each forever a turn a frame" (5., 10.) ((R.find t "A").x, (R.find t "B").y);
  let long = String.concat "\n" ("when flag clicked" :: List.init 100 (fun _ -> "change x by (1)")) in
  let t = frames 1 (R.green_flag (stage [ ("A", long) ])) in
  Alcotest.(check (float 0.)) "no loop, no yield: a hundred blocks in one frame" 100. (R.find t "A").x

let test_values () =
  let t = R.green_flag (stage [ ("A", "when flag clicked\nset [a v] to ((10) + [1])\nset [b v] to ((cat) + (1))\nset [c v] to <[CAT] = [cat]>\nset [d v] to ((-1) mod (10))\nset [e v] to (join (a) [!])") ]) in
  let t = frames 1 t in
  let v n = R.text (R.variable t n) in
  Alcotest.(check (list string)) "Scratch's arithmetic" [ "11"; "1"; "true"; "9"; "11!" ] (List.map v [ "a"; "b"; "c"; "d"; "e" ])

let test_broadcast () =
  let t = R.green_flag (stage [ ("A", "when flag clicked\nbroadcast [go v]"); ("B", "when I receive [go v]\nset x to (42)") ]) in
  Alcotest.(check (float 0.)) "sent in the first frame" 0. (R.find (frames 1 t) "B").x;
  Alcotest.(check (float 0.)) "heard in the second" 42. (R.find (frames 2 t) "B").x

let test_wait_and_pen () =
  let t = R.green_flag (stage [ ("A", "when flag clicked\npen down\nwait (1) seconds\nmove (10) steps") ]) in
  Alcotest.(check (float 0.)) "waiting, at 0.5 s" 0. (R.find (frames 15 t) "A").x;
  let t = frames 31 t in
  Alcotest.(check (float 1e-9)) "moved after the second" 10. (R.find t "A").x;
  match t.ink with
  | [ R.Line ((0., 0.), (x, _), _, _); R.Line _ ] -> Alcotest.(check (float 1e-9)) "the pen's line" 10. x
  | _ -> Alcotest.fail "a dot and a line"

let test_bounce () =
  let t = R.green_flag (stage [ ("A", "when flag clicked\ngo to x: (235) y: (0)\nif on edge, bounce") ]) in
  let a = R.find (frames 1 t) "A" in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "back inside, turned round" (220., -90.) (a.x, a.direction)

(*****************************************************************************)
(* Snap! *)
(*****************************************************************************)

(* the globals a flag script leaves, shown *)
let after_flag text names =
  let t = frames 1 (R.green_flag (stage [ ("A", text) ])) in
  List.map (fun n -> R.show t (R.variable t n)) names

let test_custom_reporter () =
  let text =
    {|define [reporter v] [factorial %n]
if <(n) < (2)> then
  report (1)
end
report ((n) * (factorial ((n) - (1))))

when flag clicked
set [r v] to (factorial (10))|}
  in
  Alcotest.(check (list string)) "recursion, by a report" [ "3628800" ] (after_flag text [ "r" ])

let test_higher_order () =
  let text =
    {|when flag clicked
set [squares v] to (map ({ (() * ()) }) over (numbers from (1) to (4)))
set [evens v] to (keep items ({ <(() mod (2)) = (0)> }) from (numbers from (1) to (10)))
set [sum v] to (combine (numbers from (1) to (5)) using ({ (() + ()) }))
set [first v] to (item (2) of (squares))|}
  in
  Alcotest.(check (list string)) "Scratch_run.mli's worked example, keep, combine, item"
    [ "(1 4 9 16)"; "(2 4 6 8 10)"; "15"; "4" ] (after_flag text [ "squares"; "evens"; "sum"; "first" ])

let test_closure () =
  let text =
    {|when flag clicked
script variables [c v]
set [c v] to (0)
set [counter v] to ({ change [c v] by (1); report (c) })
set [a v] to (call (counter))
set [b v] to (call (counter))
set [c v] to (99)
set [d v] to (call (counter))|}
  in
  Alcotest.(check (list string)) "the ring keeps the cell itself, not its value; the global c is another" [ "1"; "2"; "100"; "0" ]
    (after_flag text [ "a"; "b"; "d"; "c" ])

let test_shared_list () =
  let text = "when flag clicked\nset [a v] to (list [x] [y] [])\nset [b v] to (a)\nadd [z] to (b)\nset [n v] to (length of (a))" in
  Alcotest.(check (list string)) "one list, two names; the empty slot at the end no item" [ "3"; "(x y z)" ] (after_flag text [ "n"; "a" ])

let test_snap_round_trip () =
  let text =
    {|define [command v] [tree %size]
if <(size) > (5)> then
  move (size) steps
  tree ((size) * (0.5))
  run ({ turn right (180) degrees; move (size) steps }) with inputs (1)
end

when flag clicked
tree (40)
|}
  in
  Alcotest.(check string) "a definition, a call, a ring round a script" text (Scratch_text.print (parse text));
  let t = frames 1 (R.green_flag (stage [ ("A", text) ])) in
  Alcotest.(check int) "the custom command's lines, drawn in one frame" 0 (List.length t.ink);
  Alcotest.(check bool) "and done" true (t.threads = []);
  (* (size), the parameter: 40, 20 and 10 forward (70); then, the
     calls returning, each turns round and moves its size: 10 back
     (60), 20 on (80), 40 back *)
  let a = R.find t "A" in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "each level's own size" (40., -90.) (a.x, a.direction)

let tests =
  [
    t "scratch: Scratch_text.mli's worked example" test_worked_example;
    t "scratch: < and >, brackets and comparisons" test_angles;
    t "scratch: a project printed back as it was read" test_round_trip;
    t "scratch: errors" test_errors;
    t "scratch: Scratch_run.mli's worked example" test_run_worked_example;
    t "scratch: two forevers take turns; no loop, no yield" test_turns;
    t "scratch: numbers and texts" test_values;
    t "scratch: a broadcast heard the frame after" test_broadcast;
    t "scratch: a wait, and the pen" test_wait_and_pen;
    t "scratch: if on edge, bounce" test_bounce;
    t "snap: a custom reporter, recursive" test_custom_reporter;
    t "snap: map, keep, combine, and implicit parameters" test_higher_order;
    t "snap: a closure" test_closure;
    t "snap: a list is an object" test_shared_list;
    t "snap: a program printed back as it was read" test_snap_round_trip;
  ]
