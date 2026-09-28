(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_scheme_step.mli *)

(* each step as "before --> after", the redex and its replacement in
   brackets *)
let show (text : string) : string list =
  let mark s (i, j) = String.sub s 0 i ^ "[" ^ String.sub s i (j - i) ^ "]" ^ String.sub s j (String.length s - j) in
  let steps, error = Scheme_step.steps text in
  List.map (fun (s : Scheme_step.step) -> mark s.before s.redex ^ " --> " ^ mark s.after s.contractum) steps
  @ match error with Some e -> [ "error: " ^ e ] | None -> []

let check = Alcotest.(check (list string))

let tests =
  Testo.categorize "Scheme stepper"
    [
      Testo.create "Scheme_step.mli's example" (fun () ->
          check "steps" [ "(+ [(sq 3)] 1) --> (+ [(* 3 3)] 1)"; "(+ [(* 3 3)] 1) --> (+ [9] 1)"; "[(+ 9 1)] --> [10]" ] (show "(define (sq x) (* x x)) (+ (sq 3) 1)"));
      Testo.create "cond, a clause at a time" (fun () ->
          check "steps"
            [ "(cond [[(< 5 0)] 'neg] [else 'pos]) --> (cond [[false] 'neg] [else 'pos])";
              "[(cond [false 'neg] [else 'pos])] --> [(cond [else 'pos])]";
              "[(cond [else 'pos])] --> ['pos]" ]
            (show "(cond [(< 5 0) 'neg] [else 'pos])"));
      Testo.create "a constant's definition, then its name replaced" (fun () ->
          check "steps" [ "(define x [(+ 1 2)]) --> (define x [3])"; "(* [x] 2) --> (* [3] 2)"; "[(* 3 2)] --> [6]" ] (show "(define x (+ 1 2)) (* x 2)"));
      Testo.create "structures are values" (fun () ->
          check "steps" [ "[(posn-x (make-posn 1 2))] --> [1]" ] (show "(define-struct posn (x y)) (posn-x (make-posn 1 2))"));
      Testo.create "and, or" (fun () ->
          check "steps" [ "[(and true false)] --> [(and false)]"; "[(and false)] --> [false]" ] (show "(and true false)"));
      Testo.create "errors: the steps before, then the error" (fun () ->
          check "car" [ "(+ 1 [(* 2 2)]) --> (+ 1 [4])"; "[(+ 1 4)] --> [5]"; "error: car: expects argument of type <pair>; given 5" ] (show "(+ 1 (* 2 2)) (car 5)");
          check "lambda" [ "error: the stepper knows Beginning Student only, and lambda is not in it" ] (show "(define (adder x) (lambda (y) (+ x y)))"));
    ]
