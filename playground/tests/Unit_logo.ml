(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/Logo: the worked examples of Logo.mli, and turtle
 * geometry's theorems *)

open Logo

let t = Testo.create
let square = [ repeat 4 [ forward 100.; right 90. ] ]

(* the turtle: the last shape drawn; its position, and its heading
 * (Logo's, clockwise, from Playground's counterclockwise angle) *)
let turtle (shapes : Playground.shape list) : float * float * float =
  let s : Playground.shape = List.nth shapes (List.length shapes - 1) in
  (s.x, s.y, Float.rem (-.s.angle +. 720.) 360.)

let check_turtle msg (x, y, h) shapes =
  let (x', y', h') = turtle shapes in
  let close a b = Float.abs (a -. b) < 1e-6 || Float.abs (Float.abs (a -. b) -. 360.) < 1e-6 in
  if not (Float.abs (x -. x') < 1e-6 && Float.abs (y -. y') < 1e-6 && close h h') then
    Alcotest.failf "%s: the turtle at (%g, %g) heading %g, not (%g, %g) heading %g" msg x' y' h' x y h

let test_square () =
  let shapes = draw square in
  Alcotest.(check int) "4 lines, and the turtle" 5 (List.length shapes);
  check_turtle "back home, facing up" (0., 0., 0.) shapes;
  Alcotest.(check (float 1e-9)) "work: 4 x 100 walked, 4 x 90 degrees turned, 0.25 each" 490. (work square)

(* a closed path turns 360 degrees in all *)
let test_total_trip () =
  let circle = [ repeat 360 [ forward 1.; right 1. ] ] in
  check_turtle "a 360-gon: back where it started" (0., 0., 0.) (draw circle);
  let star = [ repeat 5 [ forward 100.; right 144. ] ] in
  check_turtle "a star: 5 x 144 = 720, twice around" (0., 0., 0.) (draw star)

(* after 150: 100 walked, 90 degrees turned (22.5), 27.5 along the
 * second side *)
let test_upto () =
  let shapes = draw_upto 150. square in
  Alcotest.(check int) "a line and a half" 3 (List.length shapes);
  check_turtle "on the second side" (27.5, 100., 90.) shapes;
  let shapes = draw_upto 110. square in
  check_turtle "turning: 10 / 0.25 = 40 degrees" (0., 100., 40.) shapes;
  Alcotest.(check int) "draw_upto infinity = draw" (List.length (draw square)) (List.length (draw_upto infinity square))

let rec koch n len =
  if n = 0 then forward len
  else
    let k = koch (n - 1) (len /. 3.) in
    block [ k; left 60.; k; right 120.; k; left 60.; k ]

let test_koch () =
  [ 0; 1; 2; 3 ]
  |> List.iter (fun n ->
         Alcotest.(check int) (Printf.sprintf "level %d: 4^%d lines" n n) (1 lsl (2 * n) + 1) (List.length (draw [ koch n 90. ]));
         check_turtle "its end: 90 pixels to the right" (90., 0., 0.) (draw [ right 90.; koch n 90.; left 90. ]))

(* Papert's state-transparent procedure: the tree of Logo.mli brings the
 * turtle back *)
let rec tree size =
  if size < 5. then stop
  else block [ forward size; left 30.; tree (size *. 0.7); right 60.; tree (size *. 0.7); left 30.; back size ]

let test_tree () = check_turtle "the tree: back home" (0., 0., 0.) (draw [ tree 100. ])

let test_pen () =
  Alcotest.(check int) "pen up: only the turtle" 1 (List.length (draw [ pen_up; repeat 4 [ forward 100.; right 90. ] ]));
  Alcotest.(check int) "hidden: only the lines" 4 (List.length (draw (hide_turtle :: square)));
  Alcotest.(check int) "filled: the region, its 4 lines, the turtle" 6 (List.length (draw [ filled Playground.red square ]));
  Alcotest.(check int) "filled, not done: not yet" 3 (List.length (draw_upto 150. [ filled Playground.red square ]))

let tests =
  Testo.categorize "Logo"
    [
      t "the square" test_square;
      t "the total turtle trip theorem" test_total_trip;
      t "draw_upto: the turtle walking" test_upto;
      t "the Koch curve: 4^n lines" test_koch;
      t "a state-transparent tree" test_tree;
      t "the pen" test_pen;
    ]
