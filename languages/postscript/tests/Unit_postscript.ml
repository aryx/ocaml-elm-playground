(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* languages/postscript *)

let t = Testo.create

(* the operand stack a program leaves, top first *)
let stack_after text = Ps_machine.stack (Ps_machine.run (Ps_machine.start text))

let failure text = match Ps_machine.status (Ps_machine.run (Ps_machine.start text)) with Ps_machine.Failed e -> e | _ -> "no error"

(* Ps_lexer.mli's worked example *)
let test_lexer () =
  let open Ps_lexer in
  Alcotest.(check bool) "/sq {dup mul} def" true
    (tokens "/sq {dup mul} def" = Ok [ Literal "sq"; Open_brace; Name "dup"; Name "mul"; Close_brace; Name "def" ]);
  Alcotest.(check bool) "numbers, brackets, strings, a comment" true
    (tokens "3 -7 .5 1e3 [a] (x \\(y\\)) % no\n" = Ok [ Int 3; Int (-7); Real 0.5; Real 1000.; Name "["; Name "a"; Name "]"; String "x (y)" ]);
  Alcotest.(check bool) "a string never closed" true (Result.is_error (tokens "(oops"))

let test_stack () =
  let check name text expected = Alcotest.(check (list string)) name expected (stack_after text) in
  check "postfix" "3 4 add 2 mul" [ "14" ];
  check "div is real, idiv an integer" "7 2 div 7 2 idiv" [ "3"; "3.5" ];
  check "a procedure is data until its name is run" "/sq {dup mul} def 5 sq /sq load" [ "{dup mul}"; "25" ];
  check "ifelse" "-4 dup 0 lt { neg } { } ifelse" [ "4" ];
  check "for" "0 1 1 10 { add } for" [ "55" ];
  check "loop and exit" "0 { 1 add dup 5 eq { exit } if } loop" [ "5" ];
  check "roll" "1 2 3 3 1 roll" [ "2"; "1"; "3" ];
  check "arrays shared, not copied" "/a [1 2] def /b a def a 0 9 put b 0 get" [ "9" ];
  check "a dictionary's scope" "/x 1 def 1 dict begin /x 2 def x end x" [ "1"; "2" ];
  check "recursion, with no frame kept for the last call" "/down { dup 0 gt { 1 sub down } if } def 100000 down" [ "0" ]

(* the LaserWriter's messages *)
let test_errors () =
  Alcotest.(check string) "undefined" "%%[ Error: undefined; OffendingCommand: sqaure ]%%" (failure "5 sqaure");
  Alcotest.(check string) "stackunderflow" "%%[ Error: stackunderflow; OffendingCommand: add ]%%" (failure "1 add");
  Alcotest.(check string) "typecheck" "%%[ Error: typecheck; OffendingCommand: add ]%%" (failure "(a) 1 add");
  Alcotest.(check string) "nocurrentpoint" "%%[ Error: nocurrentpoint; OffendingCommand: lineto ]%%" (failure "10 10 lineto")

let close name expected actual = if Float.abs (expected -. actual) > 1e-4 then Alcotest.failf "%s: %f, not %f" name actual expected

(* Ps_graphics.mli's numbers *)
let test_geometry () =
  let open Ps_graphics in
  let p0, curves = arc (0., 0.) 1. 0. 90. ~clockwise:false in
  (match curves with
  | [ ((_, k), c2, p3) ] ->
      close "k" 0.5523 k;
      (* the curve at t, from its Bernstein polynomials *)
      let (x0, y0), (x1, y1), (x2, y2), (x3, y3) = (p0, (1., k), c2, p3) in
      let radius t =
        let u = 1. -. t in
        let b0 = u *. u *. u and b1 = 3. *. u *. u *. t and b2 = 3. *. u *. t *. t and b3 = t *. t *. t in
        let x = (b0 *. x0) +. (b1 *. x1) +. (b2 *. x2) +. (b3 *. x3) and y = (b0 *. y0) +. (b1 *. y1) +. (b2 *. y2) +. (b3 *. y3) in
        Float.sqrt ((x *. x) +. (y *. y))
      in
      close "the midpoint is on the circle" 1. (radius 0.5);
      close "and the farthest it strays" 1.00027 (List.fold_left Float.max 0. (List.init 101 (fun i -> radius (float_of_int i /. 100.))))
  | _ -> Alcotest.fail "a quarter circle is one curve");
  (* 10 0 translate 90 rotate: (1, 0) lands at (10, 1) *)
  let ctm = concat (rotation 90.) (concat (translation 10. 0.) identity) in
  let x, y = transform ctm (1., 0.) in
  close "x" 10. x;
  close "y" 1. y;
  let x, y = transform (invert ctm) (10., 1.) in
  close "inverted x" 1. x;
  close "inverted y" 0. y;
  let pieces = flatten [ Move (0., 0.); Curve ((0., 100.), (100., 100.), (100., 0.)) ] in
  match pieces with
  | [ (points, false) ] -> if List.length points < 8 then Alcotest.failf "a curve cut in %d" (List.length points)
  | _ -> Alcotest.fail "one open polyline"

let test_disk () =
  List.iter
    (fun (name, text) ->
      let m = Ps_machine.run (Ps_machine.start text) in
      match Ps_machine.status m with
      | Ps_machine.Done -> if name <> "calculator" && Ps_machine.pages m = [] then Alcotest.failf "%s: no page" name
      | Ps_machine.Failed e -> Alcotest.failf "%s: %s" name e
      | Ps_machine.Running -> Alcotest.failf "%s: still running" name)
    Ps_disk.programs;
  let m = Ps_machine.run (Ps_machine.start (List.assoc "calculator" Ps_disk.programs)) in
  Alcotest.(check (list string)) "the calculator's transcript" [ "20"; "3628800"; "610"; "6"; "4"; "2" ] (Ps_machine.output m);
  let tree = Ps_machine.run (Ps_machine.start (List.assoc "tree" Ps_disk.programs)) in
  Alcotest.(check int) "the tree: a stroke per branch" 1023 (List.length (List.concat (Ps_machine.pages tree)))

let tests =
  Testo.categorize "postscript"
    [ t "lexer" test_lexer; t "stack" test_stack; t "errors" test_errors; t "geometry" test_geometry; t "disk" test_disk ]
